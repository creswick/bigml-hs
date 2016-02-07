{-# LANGUAGE OverloadedStrings #-}
module BigML.WebUtils

  where

import qualified Data.Aeson as A
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LC8
import           Network.URI (URI)
import           Network.HTTP.Client.TLS (tlsManagerSettings)

import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.MultipartFormData as HTTP
import qualified Network.HTTP.Client.OpenSSL           as HTTP
import qualified Network.HTTP.Types                    as HTTP
import           OpenSSL.Session                       as OpenSSL

import           BigML.Types

getJSON :: (A.FromJSON from)
        => String -- ^ The url
        -> String -- ^ Username
        -> String -- ^ API Key
        -> IO (Either String from)
getJSON toUrl username key = do
  mgr <- HTTP.newManager tlsManagerSettings
  req <- HTTP.parseUrl toUrl
  let req' = HTTP.setQueryString
             [ ("username", Just (C8.pack username))
             , ("api_key", Just (C8.pack key))
             ] req
  resp <- HTTP.httpLbs req' mgr
  return $ A.eitherDecode (HTTP.responseBody resp)


postJSON :: (A.ToJSON to, A.FromJSON from)
           => String -- ^ The url
           -> String -- ^ Username
           -> String -- ^ API Key
           -> to -- ^ The JSON object to POST in the HTTP body.
           -> IO (Either String from)
postJSON toUrl username key object = do
  mgr <- HTTP.newManager tlsManagerSettings
  req <- HTTP.parseUrl toUrl
  let req' = HTTP.setQueryString
             [ ("username", Just (C8.pack username))
             , ("api_key", Just (C8.pack key))
             ] req
      ct = "content-type"
      req'' = req' { HTTP.requestBody = HTTP.RequestBodyLBS $ A.encode object
                   , HTTP.requestHeaders = (ct, "application/json;charset=utf-8"):("Accept" ,"application/json;charset=utf-8")
                                           : filter (\(x, _) -> x /= ct) (HTTP.requestHeaders req)
                   , HTTP.method = "POST"
                   }
  resp <- HTTP.httpLbs req'' mgr
  return $ A.eitherDecode (HTTP.responseBody resp)

postFile :: (A.FromJSON json)
         => String -- ^ The url
         -> String -- ^ Username
         -> String -- ^ API Key
         -> FilePath -- ^ The filepath
         -> IO (Either String json)
postFile toUrl username key filepath  = do
  mgr <- HTTP.newManager tlsManagerSettings -- (HTTP.opensslManagerSettings OpenSSL.context)
  req <- HTTP.formDataBody parts =<< HTTP.parseUrl toUrl
  let req' = HTTP.setQueryString
                  [ ("username", Just $ C8.pack username)
                  , ("api_key", Just $ C8.pack key)
                  ] req

  resp <- HTTP.httpLbs req' mgr
  return $ A.eitherDecode (HTTP.responseBody resp)
  where
    parts = [ HTTP.partFileSource "file" filepath
            ]
