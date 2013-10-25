{-# LANGUAGE OverloadedStrings, DeriveGeneric, TemplateHaskell #-}
import Data.Conduit.Binary (sinkFile)
import Data.Conduit
import qualified Data.Conduit.List as CL

import AesonConfig

import Data.Aeson
import Data.Aeson.TH
import Data.Text
import GHC.Generics

import System.IO
import Control.Monad.IO.Class (liftIO, MonadIO)
import Aws.Core 
import Aws.Sign4 -- (s4Authz, Sign4)

import Control.Monad.Trans.Resource
import Network.HTTP.Conduit
import qualified Data.Conduit as C

import Data.Time.Clock
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BCL
import qualified Data.ByteString.Lazy as BL

printer :: (Show a, MonadIO m) => Sink a m ()
printer = CL.mapM_ (liftIO . print)

getSignature time headers body = 
    Sign4 {
        s4Credentials = creds,
        s4Date = time,
        s4Endpoint = "us-west-2",
        s4Service = "dynamodb",
        s4Method = "POST",
        s4Path = "/",
        s4Headers = headers,
        s4Query = [],
        s4Body = body,
        s4SgndHeaders = Nothing,
        s4CnclHeaders = Nothing
    }
    where 
        creds = Credentials { 
                    accessKeyID = "AKIAJXELXSQICXRN3VBQ", 
                    secretAccessKey = "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" 
                }

data AttributeDef =
    AttributeDef { 
        attributeName :: !Text, 
        attributeType :: !Text 
    }
        deriving (Show, Generic)

$(deriveJSON dynamoAesonOptions ''AttributeDef)

data CreateReq =
    CreateReq { 
        name :: !Text ,
        attributeDefinitions :: [AttributeDef]
    }
        deriving (Show, Generic)

$(deriveJSON dynamoAesonOptions ''CreateReq)

dynamoReq operation sink body = do
    time <- getCurrentTime
    let target = "DynamoDB_20120810." ++ operation
    let headers = [ 
                    ("Content-Encoding", "amz-1.0") ,
                    ("X-Amz-Target", B.pack target),
                    ("X-Amz-Date", "20130928T135345Z"),
                    ("User-Agent","aws-cli/1.1.0 Python/2.7.3 Linux/3.8.0-29-generic")
                ]
    let sig = getSignature time headers body
    let authHeader = s4Authz sig
    let withAuthHeader = ("Authorization", authHeader) : headers
    request <- parseUrl "http://localhost:8000/"
    let proxy = Nothing -- Just Proxy { proxyHost = "localhost", proxyPort = 8888 }
    let anotherReq = request { 
                        requestHeaders = withAuthHeader,
                        requestBody = RequestBodyBS body,
                        method = "POST",
                        proxy = proxy
    }

    withManager $ \manager -> do
        response <- http anotherReq manager
        responseBody response C.$$+- sink
    
main :: IO ()
main = do
    let createTableString = "{ \"AttributeDefinitions\": [ { \"AttributeName\": \"ForumName\", \"AttributeType\": \"S\" }, { \"AttributeName\": \"Subject\", \"AttributeType\": \"S\" }, { \"AttributeName\": \"LastPostDateTime\", \"AttributeType\": \"S\" } ], \"TableName\": \"Thread2\", \"KeySchema\": [ { \"AttributeName\": \"ForumName\", \"KeyType\": \"HASH\" }, { \"AttributeName\": \"Subject\", \"KeyType\": \"RANGE\" } ], \"LocalSecondaryIndexes\": [ { \"IndexName\": \"LastPostIndex\", \"KeySchema\": [ { \"AttributeName\": \"ForumName\", \"KeyType\": \"HASH\" }, { \"AttributeName\": \"LastPostDateTime\", \"KeyType\": \"RANGE\" } ], \"Projection\": { \"ProjectionType\": \"KEYS_ONLY\" } } ], \"ProvisionedThroughput\": { \"ReadCapacityUnits\": 5, \"WriteCapacityUnits\": 5 } }"
    putStrLn $ show createTableString 
    let myObj = CreateReq { 
                    name = "Andrew",
                    attributeDefinitions = [ AttributeDef { attributeName = "ForumName", attributeType = "S" } ]
                }
    let myString = encode myObj
    putStrLn ""
    putStrLn "My Version"
    BCL.putStrLn myString 

    putStrLn ""
    dynamoReq "CreateTable" printer createTableString 
    putStrLn ""
    dynamoReq "ListTables" printer "{}"
