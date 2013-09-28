{-# LANGUAGE OverloadedStrings #-}
import Data.Conduit.Binary (sinkFile)
import Data.Conduit
import qualified Data.Conduit.List as CL

import System.IO
import Control.Monad.IO.Class (liftIO, MonadIO)
import Aws.Core 
import Aws.Sign4 -- (s4Authz, Sign4)

import Control.Monad.Trans.Resource
import Network.HTTP.Conduit
import qualified Data.Conduit as C

import Data.Time.Clock
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as BL

printer :: (Show a, MonadIO m) => Sink a m ()
printer = CL.mapM_ (liftIO . print)

getSignature time headers = 
    Sign4 {
        s4Credentials = creds,
        s4Date = time,
        s4Endpoint = "us-west-2",
        s4Service = "dynamodb",
        s4Method = "POST",
        s4Path = "/",
        s4Headers = headers,
        s4Query = [],
        s4Body = "{}",
        s4SgndHeaders = Nothing,
        s4CnclHeaders = Nothing
    }
    where 
        creds = Credentials { 
                    accessKeyID = "AKIAJXELXSQICXRN3VBQ", 
                    secretAccessKey = "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" 
                }

listTablesBody = 
    RequestBodyLBS (BL.fromChunks $ [B.pack "{}"])

main :: IO ()
main = do
    time <- getCurrentTime
    let headers = [ 
                    ("Content-Encoding", "amz-1.0") ,
                    ("X-Amz-Target", "DynamoDB_20120810.ListTables"),
                    ("X-Amz-Date", "20130928T135345Z"),
                    ("User-Agent","aws-cli/1.1.0 Python/2.7.3 Linux/3.8.0-29-generic")
                  ]
    let sig = getSignature time headers
    let authHeader = s4Authz sig
    let withAuthHeader = ("Authorization", authHeader) : headers
    request <- parseUrl "http://localhost:8000/"
    let proxy = Just Proxy { proxyHost = "localhost", proxyPort = 8888 }
    let anotherReq = request { 
                        requestHeaders = withAuthHeader,
                        requestBody = listTablesBody,
                        method = "POST",
                        proxy = proxy
    }
    withManager $ \manager -> do
        response <- http anotherReq manager
        responseBody response C.$$+- printer
