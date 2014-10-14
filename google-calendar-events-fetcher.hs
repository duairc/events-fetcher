{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}

module Main where

-- aeson ---------------------------------------------------------------------
import           Data.Aeson
                     ( FromJSON (parseJSON)
                     , ToJSON (toJSON)
                     , Result (Error, Success)
                     , Value (Object)
                     , (.=)
                     , (.:)
                     , (.:?)
                     , decode
                     , encode
                     , fromJSON
                     , object
                     )


-- base ----------------------------------------------------------------------
import           Control.Applicative ((<$>))
import           Control.Concurrent
                     ( forkIO
                     , killThread
                     , modifyMVar_
                     , myThreadId
                     , newEmptyMVar
                     , putMVar
                     , readMVar
                     , takeMVar
                     , threadDelay
                     )
import           Control.Exception
                     ( Exception
                     , SomeException
                     , catch
                     , finally
                     , throwIO
                     )
import           Control.Monad (msum, mzero)
import           Data.Foldable (for_)
import           Data.Int (Int64)
import           Data.List (foldl')
import           Data.Monoid (mconcat, mempty)
import           Data.Traversable (for)
import           Data.Typeable (Typeable)
import           System.Environment (getArgs)
import           System.IO (stderr)


-- bytestring ----------------------------------------------------------------
import           Data.ByteString (ByteString)


-- ConfigFile ----------------------------------------------------------------
import           Data.ConfigFile
                     ( CPErrorData
                     , emptyCP
                     , get
                     , readfile
                     , sections
                     )


-- directory -----------------------------------------------------------------
import           System.Directory
                     ( createDirectoryIfMissing
                     , doesFileExist
                     , removeDirectory
                     , removeFile
                     )


-- hslogger ------------------------------------------------------------------
import           System.Log.Logger
                     ( Priority (DEBUG, WARNING)
                     , debugM
                     , errorM
                     , getLogger
                     , rootLoggerName
                     , setLevel
                     , setHandlers
                     , warningM
                     , updateGlobalLogger
                     )
import           System.Log.Handler.Simple (verboseStreamHandler)


-- http-client ---------------------------------------------------------------
import           Network.HTTP.Client (HttpException)


-- IntervalMap ---------------------------------------------------------------
import           Data.IntervalMap.Interval (Interval (IntervalCO))
import qualified Data.IntervalMap.Generic.Interval as I (Interval)
import           Data.IntervalMap.Generic.Strict
                     ( IntervalMap
                     , alter
                     , intersecting
                     , leftClosed
                     , lowerBound
                     , rightClosed
                     , upperBound
                     )


-- lens ----------------------------------------------------------------------
import           Control.Lens ((.~), (^?), (&))


-- lens-aeson ----------------------------------------------------------------
import           Data.Aeson.Lens (key, _String)


-- network -------------------------------------------------------------------
import           Network.Socket
                     ( Family (AF_UNIX)
                     , SockAddr (SockAddrUnix)
                     , Socket
                     , SocketType (Stream)
                     , accept
                     , bind
                     , listen
                     , socket
                     , sClose
                     )
import           Network.Socket.ByteString.Lazy (recv, sendAll)


-- old-locale ----------------------------------------------------------------
import           System.Locale (defaultTimeLocale)


-- text ----------------------------------------------------------------------
import           Data.Text (Text, pack, replace, unpack)
import           Data.Text.Encoding (encodeUtf8)


-- time ----------------------------------------------------------------------
import           Data.Time
                     ( Day
                     , UTCTime (UTCTime)
                     , addDays
                     , formatTime
                     , fromGregorian
                     , getCurrentTime
                     , parseTime
                     , secondsToDiffTime
                     , showGregorian
                     , toGregorian
                     , utctDay
                     )


-- unix ----------------------------------------------------------------------
import           System.Posix.Files (setFileMode)


-- wreq ----------------------------------------------------------------------
import           Network.Wreq
                     ( Response
                     , FormParam ((:=))
                     , asValue
                     , auth
                     , defaults
                     , getWith
                     , header
                     , oauth2Bearer
                     , params
                     , post
                     , responseBody
                     )


------------------------------------------------------------------------------
programName :: String
programName = "google-calendar-events-fetcher"


------------------------------------------------------------------------------
type Events = IntervalMap (Interval UTCTime) [Event]


------------------------------------------------------------------------------
data Event = Event
    { start :: !(Either Day UTCTime)
    , end :: !(Either Day UTCTime)
    , eventId :: !Text
    , htmlLink :: !Text
    , summary :: !Text
    , description :: !(Maybe Text)
    }
  deriving (Read, Show, Eq, Ord, Typeable)


------------------------------------------------------------------------------
instance I.Interval Event UTCTime where
    lowerBound = either (flip UTCTime (secondsToDiffTime 0)) id . start
    upperBound = either (flip UTCTime (secondsToDiffTime 0) . addDays 1) id . end
    leftClosed = const True
    rightClosed = const False


------------------------------------------------------------------------------
instance FromJSON Event where
    parseJSON (Object o) = do
        s <- o .: "start"
        start_ <- parseDateTime s
        e <- o .: "end"
        end_ <- parseDateTime e
        eventId_ <- o .: "id"
        htmlLink_ <- o .: "htmlLink"
        summary_ <- o .: "summary"
        description_ <- o .:? "description"
        return $ Event
            { start = start_
            , end = end_
            , eventId = eventId_
            , htmlLink = htmlLink_
            , summary = summary_
            , description = description_
            }
      where
        parseDateTime (Object d) = msum $
            [ Left <$> do
                date <- d .: "date"
                case parseTime defaultTimeLocale "%F" date of
                     Just day -> return day
                     Nothing -> mzero
            , Right <$> do
                dateTime <- d .: "dateTime"
                case parseTime defaultTimeLocale "%FT%T%Q%Z" dateTime of
                     Just time -> return time
                     Nothing -> mzero
            ]
        parseDateTime _ = mzero
    parseJSON _ = mzero


------------------------------------------------------------------------------
instance ToJSON Event where
    toJSON e = object $
        [ "start" .= dateToJSON (start e)
        , "end" .= dateToJSON (end e)
        , "id" .= eventId e
        , "summary" .= summary e
        , "htmlLink" .= htmlLink e
        ] ++ maybe [] (\d -> ["description" .= d]) (description e)
      where
        dateToJSON (Left day) = object ["date" .= pack (showGregorian day)]
        dateToJSON (Right time) = object ["dateTime" .=
            formatTime defaultTimeLocale "%FT%T%Q%z" time]


------------------------------------------------------------------------------
data Calendar = Calendar
    { clientSecret :: !Text
    , refreshToken :: !Text
    , clientId :: !Text
    , apiKey :: !Text
    , userAgent :: !Text
    , email :: !Text
    }
  deriving (Show)


------------------------------------------------------------------------------
logger :: Calendar -> String
logger calendar = programName ++ "." ++ unpack (email calendar)


------------------------------------------------------------------------------
data Config = Config
    { calendars :: [Calendar]
    , logLevel :: Priority
    }
  deriving (Show)


------------------------------------------------------------------------------
configFileName :: FilePath
configFileName = programName ++ ".conf"


------------------------------------------------------------------------------
threadDelay' :: Int64 -> IO ()
threadDelay' t = do
    let t' = t - fromIntegral (let x = maxBound in const x (threadDelay x))
    if t' > 0
        then threadDelay maxBound >> threadDelay' t'
        else threadDelay $ fromIntegral t


------------------------------------------------------------------------------
cache :: String -> Int64 -> IO a -> IO (IO a)
cache loggerName time action = do
    resultMVar <- newEmptyMVar
    _ <- forkIO $ logErrors loggerName $ do
        result <- action'
        putMVar resultMVar result
        let loop = do
             threadDelay' time
             result' <- action'
             modifyMVar_ resultMVar (const (return result'))
             loop
        loop
    return (readMVar resultMVar)
  where
    action' = logErrors loggerName action


------------------------------------------------------------------------------
data ResponseError = ResponseError (Response Value)
  deriving (Show, Typeable)


------------------------------------------------------------------------------
instance Exception ResponseError


------------------------------------------------------------------------------
data ConfigFileError
    = ConfigFileParseError FilePath CPErrorData
    | ConfigFileNotFound
  deriving (Typeable)


------------------------------------------------------------------------------
instance Show ConfigFileError where
    show (ConfigFileParseError f e) = mconcat
        [ "Could not parse config file "
        , f
        , ": "
        , show e
        ]
    show ConfigFileNotFound = mconcat
        [ "Could not find config file "
        , show configFileName
        , " in either "
        , show ("." :: String)
        , " or "
        , show ("/etc" :: String)
        ]


------------------------------------------------------------------------------
instance Exception ConfigFileError


------------------------------------------------------------------------------
fetchToken :: Calendar -> IO ByteString
fetchToken calendar = retryOnHttpException (logger calendar) $ do
    debugM (logger calendar) "fetching token"
    response <- asValue =<< post "https://accounts.google.com/o/oauth2/token"
        [ "client_secret" := clientSecret calendar
        , "grant_type" := pack "refresh_token"
        , "refresh_token" := refreshToken calendar
        , "client_id" := clientId calendar
        ]
    debugM (logger calendar) "got token"
    case response ^? responseBody . key "access_token" . _String of
        Just token' -> return $! encodeUtf8 token'
        Nothing -> throwIO (ResponseError response)


------------------------------------------------------------------------------
fetchEventsPage :: Calendar -> Maybe Text -> ByteString -> IO ([Event], Maybe Text)
fetchEventsPage calendar pageToken oathToken = retryOnHttpException (logger calendar) $ do
    debugM (logger calendar) "fetching events page"
    today <- utctDay <$> getCurrentTime
    let (year, _, _) = toGregorian today
    let timeMax = UTCTime (fromGregorian (year + 2) 1 1) (secondsToDiffTime 0)
    let opts = defaults
         & params .~
            [ ("timeMax", pack $ formatTime defaultTimeLocale "%FT%T%Q%z" timeMax)
            , ("maxResults", "2500")
            , ("singleEvents", "true")
            , ("orderBy", "starttime")
            , ("fields", "items(description,end,htmlLink,id,location,start,summary),nextPageToken")
            , ("key", apiKey calendar)
            ] ++ maybe [] (\t -> [("pageToken", t)]) pageToken
         & auth .~ oauth2Bearer oathToken
         & header "X-Javascript-User-Agent" .~ [encodeUtf8 $ userAgent calendar]
    response <- asValue =<< getWith opts (mconcat
        [ "https://www.googleapis.com/calendar/v3/calendars/"
        , unpack $ replace "@" "%40" $ email calendar
        , "/events"
        ])
    events <- case response ^? responseBody . key "items" of
        Just items -> case fromJSON items of
            Error _ -> throwIO (ResponseError response)
            Success items' -> return $! items'
        Nothing -> throwIO (ResponseError response)
    debugM (logger calendar) "got events page"
    case response ^? responseBody . key "nextPageToken" . _String of
         nextPageToken -> return $! (events, nextPageToken)


------------------------------------------------------------------------------
fetchEvents :: Calendar -> IO ByteString -> IO Events
fetchEvents calendar fetchAuthToken = do
    debugM (logger calendar) "fetching events list"
    (events, next) <- fetchAuthToken >>= fetchEventsPage calendar Nothing
    result <- go (buildMap events mempty) next
    debugM (logger calendar) "got events list"
    return result
  where
    foldEvents !imap !event = do
        let k = IntervalCO (lowerBound event) (upperBound event)
        alter (\v -> Just (event : maybe [] id v)) k imap
    buildMap events imap = foldl' foldEvents imap events
    go !imap Nothing = return imap
    go !imap next = do
        (events, next') <- fetchAuthToken >>= fetchEventsPage calendar next
        go (buildMap events imap) next'


------------------------------------------------------------------------------
retryOnHttpException :: String -> IO a -> IO a
retryOnHttpException loggerName m = catch m $ \(e :: HttpException) -> do
    warningM loggerName (show e)
    threadDelay 30000000
    retryOnHttpException loggerName m


------------------------------------------------------------------------------
data Query = Query !UTCTime !UTCTime
  deriving (Eq, Ord, Read, Show, Typeable)


------------------------------------------------------------------------------
instance FromJSON Query where
    parseJSON v = do
        (s, e) <- parseJSON v
        s' <- maybe mzero return (parseTime defaultTimeLocale "%FT%T%Q%Z" s)
        e' <- maybe mzero return (parseTime defaultTimeLocale "%FT%T%Q%Z" e)
        return $ Query s' e'


------------------------------------------------------------------------------
query :: Events -> Query -> [Event]
query imap (Query a b) = intersecting imap (IntervalCO a b) >>= snd


------------------------------------------------------------------------------
serve :: Socket -> Calendar -> IO ()
serve sock calendar = do
    token <- cache (logger calendar) 3600000000 $ fetchToken calendar
    events <- cache (logger calendar) 600000000 $ fetchEvents calendar token
    let loop = do
         (connection, from) <- accept sock
         _ <- forkIO $ flip finally (sClose connection) $ logErrors (logger calendar) $ do
            debugM (logger calendar) $ "accepted connection from " ++ show from
            bytes <- recv connection 128
            let Just q = decode bytes
            es <- events
            let result = query es q
            sendAll connection $ encode result
            debugM (logger calendar) $ "closing connection to " ++ show from
         loop
    loop


------------------------------------------------------------------------------
parseConfigFile :: FilePath -> IO Config
parseConfigFile file = do
    ecp <- readfile emptyCP file
    either (throwIO . ConfigFileParseError file . fst) return $ do
        cp <- ecp
        calendars_ <- for (sections cp) $ \email_ -> do
            clientSecret_ <- get cp email_ "secret"
            refreshToken_ <- get cp email_ "token"
            clientId_ <- get cp email_ "id"
            apiKey_ <- get cp email_ "key"
            userAgent_ <- get cp email_ "agent"
            return $ Calendar
                { clientSecret = pack clientSecret_
                , refreshToken = pack refreshToken_
                , clientId = pack clientId_
                , apiKey = pack apiKey_
                , userAgent = pack userAgent_
                , email = pack email_
                }
        logLevel_ <- either (const (return WARNING)) return $
            get cp "DEFAULT" "loglevel"
        return $ Config calendars_ logLevel_


------------------------------------------------------------------------------
main :: IO ()
main = logErrors programName $ do
    handler <- verboseStreamHandler stderr DEBUG
    _ <- getLogger programName
    updateGlobalLogger programName (setHandlers [handler])
    updateGlobalLogger rootLoggerName (setHandlers $ [] `asTypeOf` [handler])
    args <- getArgs
    file <- case args of
        file : _ -> return file
        _ -> do
            exists <- doesFileExist configFileName
            if exists then return configFileName else do
                etcExists <- doesFileExist $ "/etc/" ++ configFileName
                if etcExists then return ("/etc/" ++ configFileName) else do
                    throwIO ConfigFileNotFound
    config <- parseConfigFile file
    for_ (calendars config) (getLogger . logger)
    updateGlobalLogger programName (setLevel (logLevel config))
    let dir = "/tmp/" ++ programName
    createDirectoryIfMissing False dir
    cleanups <- for (calendars config) $ \calendar -> do
        mvar <- newEmptyMVar
        let file' = dir ++ "/" ++ unpack (email calendar) ++ ".sock"
        sock <- socket AF_UNIX Stream 0
        tid <- forkIO $ logErrors (logger calendar) $ do
            bind sock (SockAddrUnix file')
            setFileMode file' 0o666
            listen sock 5
            serve sock calendar
            putMVar mvar ()
        return (mvar, (cleanup file' sock, tid))
    flip finally (cleanupAll dir cleanups) $ do
        for_ cleanups $ takeMVar . fst
  where
    cleanup file sock = do
        sClose sock
        removeFile file
    cleanupAll dir cleanups = do
        for_ cleanups $ \(_, (cleanup_, tid)) -> do
            killThread tid
            cleanup_
        removeDirectory dir


------------------------------------------------------------------------------
logErrors :: String -> IO a -> IO a
logErrors loggerName m = do
    catch m $ \(e :: SomeException) -> do
        errorM loggerName (show e)
        myThreadId >>= killThread
        m
