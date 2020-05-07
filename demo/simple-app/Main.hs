module Main where

import Data.Aeson (FromJSON (..), ToJSON (..), genericParseJSON, genericToJSON)
import Data.Aeson.Casing (aesonPrefix, snakeCase)
import Control.Exception (Exception)
import Control.Monad.Catch (throwM)
import Data.Text (Text)
import GHC.Generics (Generic)
import Infernal (decodeRequest, encodeResponse, runSimpleLambda)
import Infernal.Internal.Logging (logDebug)
import Prelude

data DislikeNameError = DislikeNameError
  deriving stock (Eq, Show)
instance Exception DislikeNameError

data Request = Request
  { _reqName :: !Text
  } deriving stock (Eq, Generic, Show)

instance FromJSON Request where
   parseJSON = genericParseJSON (aesonPrefix snakeCase)

data Response = Response
  { _repGreeting :: !Text
  } deriving stock (Eq, Generic, Show)

instance ToJSON Response where
   toJSON = genericToJSON (aesonPrefix snakeCase)

main :: IO ()
main = do
  runSimpleLambda $ \lamReq -> do
    logDebug "In request callback"
    req <- decodeRequest lamReq
    let name = _reqName req
    logDebug ("Got name " <> name)
    -- We'll throw an unformatted exception when we encounter the magic name "Throw" just to test things out.
    case name of
      "Throw" -> throwM DislikeNameError
      _ -> pure (encodeResponse (Response ("Hello, " <> name)))
