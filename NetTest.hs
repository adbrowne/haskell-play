 {-# LANGUAGE OverloadedStrings #-}

import System.IO.Streams (InputStream, OutputStream, stdout)
import qualified System.IO.Streams as Streams
import qualified Data.ByteString as S
import Network.Http.Client
import Data.ByteString.Lazy.Char8 ()
main :: IO ()
main = do
    c <- openConnection "localhost" 8000

    q <- buildRequest $ do
        http GET "/"
        setAccept "text/html"

    sendRequest c q emptyBody

    receiveResponse c (\p i -> do
        xm <- Streams.read i
        case xm of
            Just x    -> S.putStr x
            Nothing   -> return ())

    closeConnection c
