{-| Basic logging based on co-log. -}
module Infernal.Internal.Logging
  ( SimpleLogAction
  , HasSimpleLog (..)
  , WithSimpleLog
  , defaultSimpleLogAction
  , logMsg
  , log
  , logDebug
  , logInfo
  , logWarning
  , logError
  , logException
  ) where

import Colog.Actions (richMessageAction)
import Colog.Core.Action (LogAction (..))
import Colog.Core.Severity (Severity (..))
import Colog.Message (Message, Msg (..))
import Control.Exception (Exception, displayException)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (MonadReader (..))
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Stack (HasCallStack, callStack, withFrozenCallStack)
import Lens.Micro (Lens')
import Lens.Micro.Mtl (view)
import Prelude hiding (log)

type SimpleLogAction = LogAction IO Message

class HasSimpleLog env where
  simpleLogL :: Lens' env SimpleLogAction

type WithSimpleLog env m = (MonadIO m, MonadReader env m, HasSimpleLog env, HasCallStack)

defaultSimpleLogAction :: SimpleLogAction
defaultSimpleLogAction = richMessageAction

logMsg :: WithSimpleLog env m => Message -> m ()
logMsg msg = do
  LogAction act <- view simpleLogL
  liftIO (act msg)

log :: WithSimpleLog env m => Severity -> Text -> m ()
log sev txt = withFrozenCallStack (logMsg Msg { msgStack = callStack, msgSeverity = sev, msgText = txt })

logDebug :: WithSimpleLog env m => Text -> m ()
logDebug = withFrozenCallStack (log Debug)

logInfo :: WithSimpleLog env m => Text -> m ()
logInfo = withFrozenCallStack (log Info)

logWarning :: WithSimpleLog env m => Text -> m ()
logWarning = withFrozenCallStack (log Warning)

logError :: WithSimpleLog env m => Text -> m ()
logError = withFrozenCallStack (log Error)

logException :: forall e m env . (WithSimpleLog env m, Exception e) => e -> m ()
logException = withFrozenCallStack (logError . Text.pack . displayException)
