{-# LANGUAGE OverloadedStrings #-}
module BigML.WebUtils

  where

import qualified Data.Aeson as A
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
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

postCreate :: (A.ToJSON to, A.FromJSON from) => String -> ByteString -> ByteString -> to -> IO (Either String from)
postCreate toUrl username key object = do
  mgr <- HTTP.newManager tlsManagerSettings
  req <- HTTP.parseUrl toUrl
  let req' = HTTP.setQueryString
             [ ("username", Just username)
             , ("api_key", Just key)
             ] req
      ct = "content-type"
      req'' = req' { HTTP.requestBody = HTTP.RequestBodyLBS $ A.encode object
                   , HTTP.requestHeaders = (ct, "application/json;charset=utf-8"):("Accept" ,"application/json;charset=utf-8")
                                           : filter (\(x, _) -> x /= ct) (HTTP.requestHeaders req)
                   , HTTP.method = "POST"
                   }

  -- putStrLn (show req'')
  -- putStrLn "----------------------BODY---------------------------------"
  -- putStrLn (showbody (HTTP.requestBody req''))
  -- putStrLn "----------------------BODY---------------------------------"
  resp <- HTTP.httpLbs req'' mgr
--  putStrLn ("resp received " ++ show resp)
  return $ A.eitherDecode (HTTP.responseBody resp)

showbody :: HTTP.RequestBody -> String
showbody (HTTP.RequestBodyLBS txt) = LC8.unpack txt
showbody _ = "other"

postFile :: (A.FromJSON json)
         => String -- ^ The url
         -> ByteString -- ^ Username
         -> ByteString -- ^ API Key
         -> FilePath -- ^ The filepath
         -> IO (Either String json)
postFile toUrl username key filepath  = do
  mgr <- HTTP.newManager tlsManagerSettings -- (HTTP.opensslManagerSettings OpenSSL.context)
  req <- HTTP.formDataBody parts =<< HTTP.parseUrl toUrl
  let req' = HTTP.setQueryString
                  [ ("username", Just username)
                  , ("api_key", Just key)
                  ] req

  putStrLn (show req')
  resp <- HTTP.httpLbs req' mgr
  putStrLn ("resp received " ++ show resp)
  return $ A.eitherDecode (HTTP.responseBody resp)
  where
    parts = [ HTTP.partFileSource "file" filepath
            ]
