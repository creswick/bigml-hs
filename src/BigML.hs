{-# LANGUAGE OverloadedStrings #-}
module BigML

where

import qualified Data.Aeson as A
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import           Control.Monad.Trans.Resource (runResourceT)
import           Network.URI (URI)
import           OpenSSL.Session                       as OpenSSL

import           BigML.Types
import           BigML.WebUtils

-- | Sources:
data SourceSpec = FileSource FilePath
                | RemoteSource URI
                | InlineSource A.Object

create_source :: SourceSpec -> IO (Either String CreateResponse)
create_source (FileSource fpath) = postFile "https://bigml.io/dev/source" "rcreswick" "26f9ab4099d8a0bcab2eec99cbea7f46580898bc" fpath
create_source _                  = return (Left "Unsupported source specification")


create_dataset :: SourceID -> IO (Either String CreateResponse)
create_dataset theId = postCreate "https://bigml.io/dev/dataset" "rcreswick" "26f9ab4099d8a0bcab2eec99cbea7f46580898bc" theId

-- list_sources :: IO (Either String [SourceID])
-- list_sources =

-- retrieve_source :: SourceID -> IO (Either String...)
-- update_source
-- delete_source

