{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module BigML.Types
where

import qualified Control.Exception as X
import           Data.Aeson ((.:), (.=), (.:?))
import qualified Data.Aeson as A
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time.Clock

import           GHC.Generics
-- import           System.Locale
import           Data.Time
import           Data.Time.Format

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

newtype SourceID = SourceID { sourceID :: ResourceID }
  deriving (Read, Show, Ord, Eq, Generic)

instance A.FromJSON SourceID where
  parseJSON (A.Object v) = SourceID <$> v .: "source"

instance A.ToJSON SourceID where
  toJSON (SourceID rsid) = A.object [ "source" .= rsid ]

-- | The ID of a source entry.
newtype ResourceID = ResourceID { resourceString :: String }
  deriving (Read, Show, Ord, Eq, Generic)

instance A.FromJSON ResourceID where
  parseJSON (A.String txt) = pure $ ResourceID (T.unpack txt)
  parseJSON _              = fail "Incorrect value type for ResourceID"

instance A.ToJSON ResourceID where
  toJSON (ResourceID txt) = A.String (T.pack txt)

data CreateResponse = CreateResponse
  { resp_code :: Int
  , resp_resource :: ResourceID
--, resp_object ???
  , resp_error :: Maybe String
  } deriving (Read, Show, Ord, Eq, Generic)

instance A.ToJSON CreateResponse where
  toJSON CreateResponse {..} = A.object
    [ "code" .= resp_code
    , "resource" .= resp_resource
    , "error" .= resp_error
    ]

instance A.FromJSON CreateResponse where
  parseJSON (A.Object v) = do
    resp_code <- v .: "code"
    resp_resource <- v .: "resource"
    resp_error <- v .:? "error"
    return CreateResponse {..}

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
  } deriving (Read, Show, Ord, Eq, Generic)

instance A.ToJSON Status where
  toJSON Status {..} = A.object
    [ "code" .= status_code
    , "elapsed" .= status_elapsed
    , "message" .= status_message
    ]

instance A.FromJSON Status where
  parseJSON (A.Object v) = do
    status_code <- v .: "code"
    status_elapsed <- v .:? "elapsed"
    status_message <- v .: "message"
    return Status {..}