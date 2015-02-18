module Http.Connection
  ( send )
  where

import           Network
import           Text.Printf
import           System.IO
import           System.IO.Streams (InputStream, OutputStream)
import qualified System.IO.Streams as Streams
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as Char8


-- Doesn't know anything about requests/responses. Just streams data in and out
-- to a server address and port.

send :: String -> Int -> IO (InputStream ByteString) -> IO String
send server port is = do
    h <- connectTo server $ PortNumber (fromIntegral port)
    hSetBuffering h NoBuffering
    write h is
    hGetContents h

write :: Handle -> IO (InputStream ByteString) -> IO ()
write h stream = do is <- stream
                    next <- Streams.read is
                    case next of
                        Just w  -> do hPrintf h "%s" (Char8.unpack w)
                                      -- printf "%s" (Char8.unpack w)
                                      write h (return is)
                        Nothing -> hPrintf h "\r\n"
 
listen :: Handle -> IO ()
listen h = forever $ do
    s <- hGetLine h
    putStrLn s
  where
    forever a = do a; forever a
