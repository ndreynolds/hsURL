{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Http.Request
  ( Request (..)
  , request
  , get
  , put
  , post
  , delete
  , addHeader
  , removeHeader
  , setBody
  , setBodyString
  , setBodyFile
  , buildRequest 
  , makeRequest )
  where


import           Http.Client
import           Http.Connection
import           Http.Uri
import qualified Http.Response
import           Control.Monad
import           System.IO (Handle, openFile, IOMode (..))
import           System.IO.Streams (InputStream, OutputStream)
import qualified System.IO.Streams as Streams
import           Text.Printf
import           Data.List hiding (delete)
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as Char8


data Request = Request 
    { method :: String
    , uri :: String
    , params :: [(String,String)]
    , headers :: [(String,String)]
    , body :: RequestBody
    , version :: String
    }

data RequestBody = String String
                 | ByteString ByteString
                 | Handle (IO Handle)

request :: String -> String -> Request
request method uri = Request { method=method
                             , uri=uri
                             , params=[]
                             , headers=[]
                             , body=emptyBody
                             , version="HTTP/1.1"
                             }

emptyBody :: RequestBody
emptyBody = String ""

get    = request "GET"
post   = request "POST"
put    = request "PUT"
delete = request "DELETE"

crlf = "\r\n"

-- TODO: If they've included a request body, we should set the Content-Length
-- header---provided, of course, that we're not streaming.

defaultHeaders :: Request -> [(String,String)]
defaultHeaders req = [ ("Accept"     , "*/*")
                     , ("User-Agent" , "hsURL")
                     , ("Connection" , "close")
                     , ("Host"       , (host . uriParts . uri) req)
                     ]

addHeader :: Request -> (String,String) -> Request 
addHeader req header = req { headers = header : headers req }

hasHeader :: Request -> String -> Bool
hasHeader req key = any pred (headers req)
    where pred x = fst x == key

removeHeader :: Request -> String -> Request
removeHeader req key = req { headers = filter pred (headers req) }
    where pred x = fst x /= key

setBody :: Request -> RequestBody -> Request
setBody req body = req { body = body }

setBodyString :: Request -> String -> Request
setBodyString req body = req { body = String body }

setBodyFile :: Request -> FilePath -> Request
setBodyFile req path = req { body = Handle $ openFile path ReadMode }

setDefaultHeaders :: Request -> Request
setDefaultHeaders req = req { headers = newHdrs ++ headers req }
    where notSet hdr = not $ hasHeader req $ fst hdr
          newHdrs = filter notSet $ defaultHeaders req

buildRequestLine :: Request -> String
buildRequestLine req = unwords [method req, p, version req]
    where p = path . uriParts . uri $ req

buildHeaders :: Request -> String
buildHeaders req = intercalate crlf (map printHdr $ headers req)
    where printHdr (k,v) = k ++ ": " ++ v

buildBody :: Request -> IO (InputStream ByteString)
buildBody req = case body req of 
    Handle h     -> do uh <- h; Streams.handleToInputStream uh
    String s     -> Streams.fromByteString $ Char8.pack s
    ByteString b -> Streams.fromByteString b

buildHead :: Request -> IO (InputStream ByteString)
buildHead req = Streams.fromByteString $ Char8.pack (raw ++ crlf)
    where req' = setDefaultHeaders req
          raw = intercalate crlf [buildRequestLine req', buildHeaders req']

buildRequest :: Request -> IO (InputStream ByteString)
buildRequest req = do head <- buildHead req
                      body <- buildBody req
                      Streams.concatInputStreams [head,body]

makeRequest :: Request -> IO (Http.Response.Response)
makeRequest req = liftM Http.Response.response (send h p (buildRequest req))
    where parts = uriParts (uri req)
          h = host parts
          p = port parts
