{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
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
import           Data.Foldable (for_, toList)
import           Data.Int (Int64)
import           Data.List (foldl')
import           Data.Maybe (mapMaybe)
import           Data.Monoid ((<>), mconcat, mempty)
import           Data.Traversable (for, traverse)
import           Data.Typeable (Typeable)
import           Data.Unique (newUnique, hashUnique)
import           Prelude hiding (log)
import           System.Environment (getArgs)
import           System.IO (stderr)


-- bytestring ----------------------------------------------------------------
import           Data.ByteString.Lazy (fromStrict)


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
import           Network.HTTP.Client
                     ( HttpException
                     , applyBasicAuth
                     , httpLbs
                     , parseUrl
                     , withManager
                     )
import           Network.HTTP.Client.Internal
                     ( RequestBody (RequestBodyBS)
                     , method
                     , requestBody
                     , requestHeaders
                     )
import qualified Network.HTTP.Client.Internal as H
                     ( responseBody
                     )


-- http-client-tls -----------------------------------------------------------
import           Network.HTTP.Client.TLS
                     ( tlsManagerSettings
                     )


-- iCalendar -----------------------------------------------------------------
import           Text.ICalendar
                     ( Date (Date)
                     , DateTime (FloatingDateTime, UTCDateTime, ZonedDateTime)
                     , DTStart (DTStartDate, DTStartDateTime)
                     , DTEnd (DTEndDate, DTEndDateTime)
                     , DurationProp (DurationProp)
                     , Duration (DurationDate, DurationTime, DurationWeek)
                     , Sign (Positive)
                     , descriptionValue
                     , locationValue
                     , parseICalendar
                     , summaryValue
                     , uidValue
                     , urlValue
                     , vcEvents
                     , veDescription
                     , veDTEndDuration
                     , veDTStart
                     , veLocation
                     , veSummary
                     , veUID
                     , veUrl
                     )


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
import           Control.Lens ((.~), (?~), (^?), (&), (^..))


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


-- network-uri ---------------------------------------------------------------
import           Network.URI (uriToString)



-- old-locale ----------------------------------------------------------------
import           System.Locale (defaultTimeLocale)


-- text ----------------------------------------------------------------------
import           Data.Text
                     ( Text
                     , intercalate
                     , isPrefixOf
                     , pack
                     , replace
                     , splitOn
                     , unpack
                     )
import           Data.Text.Lazy (toStrict)
import           Data.Text.Encoding (encodeUtf8)


-- time ----------------------------------------------------------------------
import           Data.Time
                     ( Day
                     , UTCTime (UTCTime)
                     , addDays
                     , addUTCTime
                     , formatTime
                     , fromGregorian
                     , getCurrentTime
                     , getCurrentTimeZone
                     , localTimeToUTC
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


-- xml-conduit ---------------------------------------------------------------
import           Text.XML (def, parseLBS_)


-- xml-lens ------------------------------------------------------------------
import           Text.XML.Lens (named, nodes, root, _Element, _Content)


------------------------------------------------------------------------------
programName :: String
programName = "events-fetcher"


------------------------------------------------------------------------------
type Events = IntervalMap (Interval UTCTime) [Event]


------------------------------------------------------------------------------
data Event = Event
    { start :: !(Either Day UTCTime)
    , end :: !(Either Day UTCTime)
    , eventId :: !Text
    , summary :: !Text
    , htmlLink :: !(Maybe Text)
    , location :: !(Maybe Text)
    , description :: !(Maybe Text)
    }
  deriving (Read, Show, Eq, Ord, Typeable)


------------------------------------------------------------------------------
instance I.Interval Event UTCTime where
    lowerBound = either (flip UTCTime (secondsToDiffTime 0)) id . start
    upperBound = either (flip UTCTime (secondsToDiffTime 0)) id . end
    leftClosed = const True
    rightClosed = const False


------------------------------------------------------------------------------
instance FromJSON Event where
    parseJSON (Object o) = do
        s <- o .: "start"
        start_ <- parseDateTime s
        e <- o .: "end"
        end_ <- either (Left . addDays 1) Right <$> parseDateTime e
        eventId_ <- o .: "id"
        summary_ <- o .: "summary"
        htmlLink_ <- o .:? "htmlLink"
        location_ <- o .:? "location"
        description_ <- o .:? "description"
        return $ Event
            { start = start_
            , end = end_
            , eventId = eventId_
            , summary = summary_
            , htmlLink = htmlLink_
            , location = location_
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
    toJSON e = object $ mconcat
        [
            [ "start" .= dateToJSON (start e)
            , "end" .= dateToJSON (end e)
            , "id" .= eventId e
            , "summary" .= summary e
            ]
        , maybe [] (\l -> ["htmlLink" .= l]) (htmlLink e)
        , maybe [] (\l -> ["location" .= l]) (location e)
        , maybe [] (\d -> ["description" .= d]) (description e)
        ]
      where
        dateToJSON (Left day) = object ["date" .= pack (showGregorian day)]
        dateToJSON (Right time) = object ["dateTime" .=
            formatTime defaultTimeLocale "%FT%T%Q%z" time]


------------------------------------------------------------------------------
data GoogleCalendar = GoogleCalendar
    { clientSecret :: !Text
    , refreshToken :: !Text
    , clientId :: !Text
    , apiKey :: !Text
    , userAgent :: !Text
    , email :: !Text
    }
  deriving (Show)


------------------------------------------------------------------------------
data CaldavCalendar = CaldavCalendar
    { url :: !Text
    , username :: !Text
    , password :: !Text
    }
  deriving (Show)


------------------------------------------------------------------------------
data Calendar = Google GoogleCalendar | Caldav CaldavCalendar
  deriving (Show)


------------------------------------------------------------------------------
name :: Calendar -> String
name (Google calendar) = unpack (email calendar)
name (Caldav calendar) = map sanitize $ unpack (url calendar)
  where
    sanitize '/' = '_'
    sanitize ':' = '_'
    sanitize '%' = '_'
    sanitize c = c


------------------------------------------------------------------------------
logger :: Calendar -> String
logger calendar = programName ++ "." ++ name calendar


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
twoYearsFromNow :: IO UTCTime
twoYearsFromNow = do
    today <- utctDay <$> getCurrentTime
    let (year, _, _) = toGregorian today
    return $ UTCTime (fromGregorian (year + 2) 1 1) (secondsToDiffTime 0)


------------------------------------------------------------------------------
fetchGoogleEvents :: GoogleCalendar -> IO Events
fetchGoogleEvents calendar = do
    debugM log "fetching events list"
    token <- fetchToken
    (events, next) <- fetchPage Nothing token
    result <- go token (buildMap mempty events) next
    debugM log "got events list"
    return result
  where
    log = logger (Google calendar)
    go _ !imap Nothing = return imap
    go token !imap next = do
        (events, next') <- fetchPage next token
        go token (buildMap imap events) next'
    fetchToken = retryOnHttpException log $ do
        debugM log "fetching token"
        response <- asValue =<< post "https://accounts.google.com/o/oauth2/token"
            [ "client_secret" := clientSecret calendar
            , "grant_type" := pack "refresh_token"
            , "refresh_token" := refreshToken calendar
            , "client_id" := clientId calendar
            ]
        debugM log "got token"
        case response ^? responseBody . key "access_token" . _String of
            Just token' -> return $! encodeUtf8 token'
            Nothing -> throwIO (ResponseError response)
    fetchPage pageToken oathToken = retryOnHttpException log $ do
        debugM log "fetching events page"
        timeMax <- twoYearsFromNow
        let opts = defaults
             & params .~
                [ ("timeMax", pack $ formatTime defaultTimeLocale "%FT%T%Q%z" timeMax)
                , ("maxResults", "2500")
                , ("singleEvents", "true")
                , ("orderBy", "starttime")
                , ("fields", "items(description,end,htmlLink,id,location,start,summary),nextPageToken")
                , ("key", apiKey calendar)
                ] ++ maybe [] (\t -> [("pageToken", t)]) pageToken
             & auth ?~ oauth2Bearer oathToken
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
        debugM log "got events page"
        case response ^? responseBody . key "nextPageToken" . _String of
            nextPageToken -> return $! (events, nextPageToken)


------------------------------------------------------------------------------
fetchCaldavEvents :: CaldavCalendar -> IO Events
fetchCaldavEvents calendar = do
    debugM log "fetching events list"
    time <- twoYearsFromNow
    let timeBS = encodeUtf8 $ pack $ formatTime defaultTimeLocale "%Y%m%dT%H%M%SZ" time
    let contentType = "application/xml; charset=\"utf-8\""
    let body = RequestBodyBS $ mconcat
         [ "<?xml version=\"1.0\" encoding=\"utf-8\" ?>\n\
           \<C:calendar-query xmlns:D=\"DAV:\" xmlns:C=\"urn:ietf:params:xml:ns:caldav\">\n\
           \  <D:prop>\n\
           \    <D:getetag/>\n\
           \    <C:calendar-data>\n\
           \      <C:expand start=\"10000101T000000Z\" end=\""
         , timeBS
         , "\"/>\n\
           \    </C:calendar-data>\n\
           \  </D:prop>\n\
           \  <C:filter>\n\
           \    <C:comp-filter name=\"VCALENDAR\">\n\
           \      <C:comp-filter name=\"VEVENT\">\n\
           \        <C:time-range start=\"10000101T000000Z\" end=\""
         , timeBS
         , "\"/>\n\
           \      </C:comp-filter>\n\
           \    </C:comp-filter>\n\
           \  </C:filter>\n\
           \</C:calendar-query>"
         ]
    let user = encodeUtf8 $ username calendar
    let pass = encodeUtf8 $ password calendar
    req <- parseUrl (unpack $ url calendar)
    let req' = applyBasicAuth user pass req
    let req'' = req'
         { method = "REPORT"
         , requestHeaders = requestHeaders (req') ++ 
            [ ("Depth", "1")
            , ("Content-Type", contentType)
            ]
         , requestBody = body
         }
    rsp <- withManager tlsManagerSettings $ httpLbs req''
    debugM log "got response; parsing XML..."
    let xml = parseLBS_ def (H.responseBody rsp)
    let icals = xml ^.. root 
         . nodes . traverse . _Element
         . nodes . traverse . _Element
         . nodes . traverse . _Element
         . nodes . traverse . _Element
         . named "calendar-data"
         . nodes . traverse . _Content
    icals' <- mapM sanitize icals
    debugM log "parsing iCal..."
    let vevents = toList $ vcEvents $ mconcat $ map parse icals'
    zone <- getCurrentTimeZone
    debugM log "building index..."
    let events = buildMap mempty $ mapMaybe (v2e zone) vevents
    debugM log "got events list"
    return events
  where
    log = logger (Caldav calendar)
    parse = toVCal . parseICalendar def programName . toLBS
    toVCal (Right (icals, _)) = mconcat icals
    toVCal (Left s) = error s
    toLBS = fromStrict . encodeUtf8

    sanitize = fmap (intercalate "\r\n")
        . mapM randomiseUid
        . filter (not . isPrefixOf "RECURRENCE-ID:")
        . splitOn "\r\n"

    randomiseUid l | "UID:" `isPrefixOf` l = do
        ("UID:" <>) . pack . show . hashUnique <$> newUnique
    randomiseUid l = return l

    v2e z v = do
        start_ <- veDTStart v
        end_ <- veDTEndDuration v
        let start_' = dtStart z start_
        let end_' = dtEnd z start_' end_
        return $ Event
            { start = start_'
            , end = end_'
            , eventId = toStrict $ uidValue $ veUID v
            , summary = toStrict $ maybe "[untitled]" summaryValue $ veSummary v
            , htmlLink = (\x -> pack $ uriToString id x "") . urlValue <$> veUrl v
            , location = toStrict . locationValue <$> veLocation v
            , description = toStrict . descriptionValue <$> veDescription v
            }

    dtStart z (DTStartDateTime x _) = Right $ dt2utc z x
    dtStart _ (DTStartDate (Date x) _) = Left x

    dtEnd z _ (Left (DTEndDateTime x _)) = Right $ dt2utc z x
    dtEnd _ _ (Left (DTEndDate (Date x) _)) = Left x
    dtEnd _ s (Right (DurationProp d _)) = Right $ addUTCTime (d2dt d) (edu2utc s)

    dt2utc _ (UTCDateTime x) = x
    dt2utc z (FloatingDateTime x) = localTimeToUTC z x
    dt2utc z (ZonedDateTime x _) = localTimeToUTC z x

    edu2utc (Left x) = UTCTime x (secondsToDiffTime 0)
    edu2utc (Right x) = x

    d2dt (DurationDate p d h m s) = fromIntegral $ s2m p * (((((d * 24) + h) * 60) + m) * 60) + s
    d2dt (DurationTime p h m s) = fromIntegral $ s2m p * (((h * 60) + m) * 60) + s
    d2dt (DurationWeek p w) = fromIntegral $ s2m p + (w * 7 * 24 * 60 * 60)

    s2m Positive = 1
    s2m _ = negate 1

{-
data Event = Event
    { start :: !(Either Day UTCTime)
    , end :: !(Either Day UTCTime)
    , eventId :: !Text
    , summary :: !Text
    , htmlLink :: !(Maybe Text)
    , location :: !(Maybe Text)
    , description :: !(Maybe Text)
    }
-}


------------------------------------------------------------------------------
buildMap :: Events -> [Event] -> Events
buildMap = foldl' f
  where
    f !imap !event = do
        let k = IntervalCO (lowerBound event) (upperBound event)
        alter (\v -> Just (event : maybe [] id v)) k imap


------------------------------------------------------------------------------
fetchEvents :: Calendar -> IO Events
fetchEvents (Google calendar) = fetchGoogleEvents calendar
fetchEvents (Caldav calendar) = fetchCaldavEvents calendar


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
    events <- cache (logger calendar) 100000000 $ fetchEvents calendar
    let loop = do
         (connection, _) <- accept sock
         _ <- forkIO $ flip finally (sClose connection) $ logErrors (logger calendar) $ do
            debugM (logger calendar) $ "accepted connection"
            bytes <- recv connection 128
            let Just q = decode bytes
            es <- events
            debugM (logger calendar) $ "received request: " ++ show q
            let result = query es q
            debugM (logger calendar) $ "sending response: " ++ show result
            sendAll connection $ encode result
            debugM (logger calendar) $ "closing connection"
         loop
    loop


------------------------------------------------------------------------------
parseConfigFile :: FilePath -> IO Config
parseConfigFile file = do
    ecp <- readfile emptyCP file
    either (throwIO . ConfigFileParseError file . fst) return $ do
        cp <- ecp
        calendars_ <- for (sections cp) $ \email_ -> msum
            [ do
                clientSecret_ <- get cp email_ "secret"
                refreshToken_ <- get cp email_ "token"
                clientId_ <- get cp email_ "id"
                apiKey_ <- get cp email_ "key"
                userAgent_ <- get cp email_ "agent"
                return $ Google $ GoogleCalendar
                    { clientSecret = pack clientSecret_
                    , refreshToken = pack refreshToken_
                    , clientId = pack clientId_
                    , apiKey = pack apiKey_
                    , userAgent = pack userAgent_
                    , email = pack email_
                    }
            , do
                username_ <- get cp email_ "username"
                password_ <- get cp email_ "password"
                return $ Caldav $ CaldavCalendar
                    { url = pack email_
                    , username = pack username_
                    , password = pack password_
                    }
            ]
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
        let file' = dir ++ "/" ++ name calendar ++ ".sock"
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
