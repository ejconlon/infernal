module Main where

import Data.Aeson (FromJSON, ToJSON)
import Control.Exception (Exception)
import Control.Monad.Catch (throwM)
import Data.Text (Text)
import GHC.Generics (Generic)
import Infernal (decodeRequest, encodeResponse, runSimpleLambda)
import LittleLogger (logDebug)
import Prelude

data DislikeNameError = DislikeNameError
  deriving stock (Eq, Show)
instance Exception DislikeNameError

data Request = Request
  { name :: !Text
  } deriving stock (Eq, Generic, Show)

instance FromJSON Request

data Response = Response
  { greeting :: !Text
  } deriving stock (Eq, Generic, Show)

instance ToJSON Response

main :: IO ()
main = do
  runSimpleLambda $ \lamReq -> do
    logDebug "In request callback"
    Request theirName <- decodeRequest lamReq
    logDebug ("Got name " <> theirName)
    -- We'll throw an unformatted exception when we encounter the magic name "Throw" just to test things out.
    case theirName of
      "Throw" -> throwM DislikeNameError
      _ -> pure (encodeResponse (Response ("Hello, " <> theirName)))
