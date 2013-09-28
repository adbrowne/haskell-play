 module Example where
 
 import Data.Conduit
 import qualified Data.Conduit.List as CL
 import Control.Monad.IO.Class (liftIO)
 import Control.Monad.Trans.Class (lift)
 
 import AWS
 import AWS.EC2
 import qualified AWS.EC2.Util as Util
 
 main :: IO ()
 main = do
     cred <- loadCredential
     doc <- runResourceT $
         runEC2 cred $
             Util.list $ describeInstances [] []
     print doc
     putStr "Length: "
     print $ length doc
