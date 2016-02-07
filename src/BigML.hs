{-# LANGUAGE OverloadedStrings #-}
module BigML

where

import           Control.Monad.State
import qualified Data.Aeson as A
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import           Control.Monad.Trans.Resource (runResourceT)
import           Network.URI (URI)
import           OpenSSL.Session as OpenSSL

import           BigML.Types
import           BigML.WebUtils

-- | Run a BigML action.
runBigML :: BigMLState -> BigML a -> IO a
runBigML init actions = do
  (res, _) <- runStateT actions init
  return res

-- | Different means of specifying a source:
data SourceSpec = FileSource FilePath
                | RemoteSource URI
                | InlineSource A.Object

-- | Create a source from a file, uri, or in-line object. (Only files are supported at the moment.)
create_source :: SourceSpec -> BigML (Either String CreateResponse)
create_source (FileSource fpath) = do
  uname <- getUsername
  key <- getApiKey
  sourceUrl <- getSourceUrl
  liftIO $ postFile sourceUrl uname key fpath
create_source _                  = return (Left "Unsupported source specification")

-- | Create a dataset from a given 'SourceID'
create_dataset :: SourceID -> BigML (Either String CreateResponse)
create_dataset theId =  do
  uname <- getUsername
  key <- getApiKey
  datasetUrl <- getDatasetUrl
  liftIO $ postJSON datasetUrl uname key theId

