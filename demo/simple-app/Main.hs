module Main where

import Infernal (decodeRequest, encodeResponse, runSimpleLambda)
import Heart.App.Logging (logDebug)
import Heart.Core.Aeson (AesonRecord (..))
import Heart.Core.Prelude

data DislikeNameError = DislikeNameError
  deriving stock (Eq, Show, Typeable)
instance Exception DislikeNameError

data Request = Request
  { _reqName :: !Text
  } deriving stock (Eq, Show, Generic)
    deriving FromJSON via (AesonRecord Request)

data Response = Response
  { _repGreeting :: !Text
  } deriving stock (Eq, Show, Generic)
    deriving ToJSON via (AesonRecord Response)

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
