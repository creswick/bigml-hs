{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module BigML.Types
where

import           Control.Concurrent (threadDelay)
import qualified Control.Exception as X
import           Control.Monad.State
import           Data.Aeson ((.:), (.=), (.:?))
import qualified Data.Aeson as A
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time.Clock

import           GHC.Generics
-- import           System.Locale
import           Data.Time
import           Data.Time.Format

-- | The 'BigML' monad uses this state type to store the username, api
-- key, and base url (which controls the dev / production nature of
-- your requests).
data BigMLState = BigMLState { username :: String
                             , api_key :: String
                             , base_url :: String
                             , poll_delay :: Int -- ^ The time to wait between polling requests, in microseconds.
                             } deriving (Read, Show)

-- | A default initial state for issuing development queries.
devState :: BigMLState
devState = BigMLState { username = ""
                      , api_key = ""
                      , base_url = "https://bigml.io/dev"
                      , poll_delay = 5000000 -- 5 seconds
                      }

-- | A default initial state for production.
prodState :: BigMLState
prodState = BigMLState { username = ""
                       , api_key = ""
                       , base_url = "https://bigml.io"
                       , poll_delay = 5000000 -- 5 seconds
                       }

-- | The BigML monad.
type BigML a = StateT BigMLState IO a

-- | Wait for the `poll_delay`
doDelay :: BigML ()
doDelay = do
  delay <- gets poll_delay
  liftIO $ threadDelay delay

-- | Accessor for the username
getUsername :: BigML String
getUsername = gets username

-- | Accessor for the API key
getApiKey :: BigML String
getApiKey = gets api_key

getSourceUrl :: BigML String
getSourceUrl = do
  url <- gets base_url
  return (url ++ "/source")

getDatasetUrl :: BigML String
getDatasetUrl = do
  url <- gets base_url
  return (url  ++ "/dataset")

getModelUrl :: BigML String
getModelUrl = do
  url <- gets base_url
  return (url  ++ "/model")

getStatusUrl :: ResourceID -> BigML String
getStatusUrl theId = do
  url <- gets base_url
  return (url ++ "/" ++ resourceString theId)

-- | Parse UTC Time from JSON encoded time strings, such as: "2016-02-06T19:24:23.705484"
parseUTCTime :: String -> Either String UTCTime
parseUTCTime dateString =
  case parseTimeM True defaultTimeLocale timeformat dateString of
    Nothing  -> Left ("Could not parse time: '" ++ dateString ++"'")
    Just utc -> Right utc

timeformat :: String
timeformat = "%0Y-%m-%dT%H:%M:%S%Q"

-- | The dual of parseUTCTime.
formatUTCTime :: UTCTime -> String
formatUTCTime time = formatTime defaultTimeLocale timeformat time

-- | A ResourceID specifically for a source.
newtype SourceID = SourceID ResourceID
  deriving (Read, Show, Ord, Eq, Generic)

instance A.FromJSON SourceID where
  parseJSON (A.Object v) = SourceID <$> v .: "source"

instance A.ToJSON SourceID where
  toJSON (SourceID rsid) = A.object [ "source" .= rsid ]

-- | A ResourceID specifically for a dataset.
newtype DatasetID = DatasetID ResourceID
  deriving (Read, Show, Ord, Eq, Generic)

instance A.FromJSON DatasetID where
  parseJSON (A.Object v) = DatasetID <$> v .: "dataset"

instance A.ToJSON DatasetID where
  toJSON (DatasetID rsid) = A.object [ "dataset" .= rsid ]

-- | The ID of a source entry.
newtype ResourceID = ResourceID { resourceString :: String }
  deriving (Read, Show, Ord, Eq, Generic)

instance A.FromJSON ResourceID where
  parseJSON (A.String txt) = pure $ ResourceID (T.unpack txt)
  parseJSON _              = fail "Incorrect value type for ResourceID"

instance A.ToJSON ResourceID where
  toJSON (ResourceID txt) = A.String (T.pack txt)

-- | A generic response type for creating resources.
--
-- TODO This could be parameterized over the response resource ID type.
data CreateResponse = CreateResponse
  { resp_code :: Int
  , resp_resource :: ResourceID
--, resp_object ???
  , resp_error :: Maybe String
  , resp_status :: Status
  } deriving (Read, Show, Ord, Eq, Generic)

instance A.ToJSON CreateResponse where
  toJSON CreateResponse {..} = A.object
    [ "code" .= resp_code
    , "resource" .= resp_resource
    , "error" .= resp_error
    , "status" .= resp_status
    ]

instance A.FromJSON CreateResponse where
  parseJSON (A.Object v) = do
    resp_code <- v .: "code"
    resp_resource <- v .: "resource"
    resp_error <- v .:? "error"
    resp_status <- v .: "status"
    return CreateResponse {..}

-- legacy parsers from getting some of the full-object details.
--
-- TODO: find a canonical specification for the JSON objects that come back,
-- so this isn't just guess-work.

data SourceParser = SourceParser
  { sp_header :: Maybe Bool
  , sp_locale :: Maybe String
  , sp_missing_tokens :: Maybe [String]
  , sp_quote :: Maybe String
  , sp_separator :: Maybe String
  , sp_trim :: Maybe Bool
  } deriving (Read, Show, Ord, Eq, Generic)

instance A.ToJSON SourceParser where
  toJSON SourceParser {..} = A.object
    [ "header"         .= sp_header
    , "locale"         .= sp_locale
    , "missing_tokens" .= sp_missing_tokens
    , "quote"          .= sp_quote
    , "separator"      .= sp_separator
    , "trim"           .= sp_trim
    ]

instance A.FromJSON SourceParser where
  parseJSON (A.Object v) = do
    sp_header <- v .:? "header"
    sp_locale <- v .:? "locale"
    sp_missing_tokens <- v .:? "missing_tokens"
    sp_quote <- v .:? "quote"
    sp_separator <- v .:? "separator"
    sp_trim <- v .:? "trim"
    return SourceParser {..}

data Status = Status
  { status_code :: Int
  , status_elapsed :: Maybe Int
  , status_message :: String
  , status_progress :: Maybe Double
  , status_task :: Maybe String
  } deriving (Read, Show, Ord, Eq, Generic)

instance A.ToJSON Status where
  toJSON Status {..} = A.object
    [ "code" .= status_code
    , "elapsed" .= status_elapsed
    , "message" .= status_message
    , "progress" .= status_progress
    , "task" .= status_task
    ]

instance A.FromJSON Status where
  parseJSON (A.Object v) = do
    status_code <- v .: "code"
    status_elapsed <- v .:? "elapsed"
    status_message <- v .: "message"
    status_progress <- v .:? "progress"
    status_task <- v .:? "task"
    return Status {..}
