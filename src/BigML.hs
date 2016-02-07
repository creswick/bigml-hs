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
  url <- getSourceUrl
  liftIO $ postFile url uname key fpath
create_source _                  = return (Left "Unsupported source specification")

-- | Create a dataset from a given 'SourceID'
create_dataset :: SourceID -> BigML (Either String CreateResponse)
create_dataset theId =  do
  uname <- getUsername
  key <- getApiKey
  url <- getDatasetUrl
  liftIO $ postJSON url uname key theId

-- | Create a model from a given 'DatasetID'
create_model :: DatasetID -> BigML (Either String CreateResponse)
create_model theId =  do
  uname <- getUsername
  key <- getApiKey
  url <- getModelUrl
  liftIO $ postJSON url uname key theId

-- | Retrieve the status for a given resource.
check_resource :: ResourceID -> BigML (Either String Status)
check_resource theId = do
  uname <- getUsername
  key <- getApiKey
  url <- getStatusUrl theId
  resp <- liftIO $ getJSON url uname key
  return (resp_status `fmap` resp)

-- | Wait for a resource to return a status task of "Done", then run a BigML Action.
--
-- This allows you to sequence asynchronous actions, waiting until a
-- polling action indicates that the specified resource is ready.
whenReady :: ResourceID -> BigML (Either String a) -> BigML (Either String a)
whenReady theId action = do
  eRes <- check_resource theId
  case eRes of
    Left     err -> return $ Left err
    Right status -> case status_task status of
                      (Just "Done") -> doDelay >> action
                      _ -> whenReady theId action

