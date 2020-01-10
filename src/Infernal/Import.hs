{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Infernal.Import where

import Control.Monad (forever)
import Data.ByteString (ByteString)
import qualified Data.Text as Text
import Infernal.Prelude
import qualified Network.HTTP.Client as Http
import System.Exit (ExitCode)
import UnliftIO.Environment (getEnv)

newtype LambdaRequestId = LambdaRequestId
  { _unLambdaRequestId :: ByteString
  } deriving (Eq, Show)

$(makeLenses ''LambdaRequestId)

data LambdaRequest = LambdaRequest
  { _lreqId :: !LambdaRequestId
  , _lreqBody :: !ByteString
  }

$(makeLenses ''LambdaRequest)

data LambdaError = LambdaError
  { _lerrType :: !Text
  , _lerrMessage :: !Text
  } deriving (Eq, Show, Typeable)

$(makeLenses ''LambdaError)

instance Exception LambdaError

defaultLambdaError :: LambdaError
defaultLambdaError = LambdaError "InternalError" "No information is available."

type GetLambdaRequest m = m LambdaRequest
type PostLambdaInitError m = LambdaError -> m ()
type PostLambdaInvokeError m = LambdaRequestId -> LambdaError -> m ()
type PostLambdaResponse m = LambdaRequestId -> ByteString -> m ()

data LambdaClient m = LambdaClient
  { _lcGetLambdaRequest :: !(GetLambdaRequest m)
  , _lcPostLambdaInitError :: !(PostLambdaInitError m)
  , _lcPostLambdaInvokeError :: !(PostLambdaInvokeError m)
  , _lcPostLambdaResponse :: !(PostLambdaResponse m)
  }

$(makeLenses ''LambdaClient)

type RunCallback n = LambdaRequest -> n ByteString
type InvokeErrorCallback n = LambdaRequest -> SomeException -> n LambdaError
type InitErrorCallback n = SomeException -> n LambdaError
type UncaughtErrorCallback n = SomeException -> n ()

data CallbackConfig n = CallbackConfig
  { _cbcRunCallback :: !(RunCallback n)
  , _cbcInvokeErrorCallback :: !(InvokeErrorCallback n)
  , _cbcInitErrorCallback :: !(InitErrorCallback n)
  , _cbcUncaughtErrorCallback :: !(UncaughtErrorCallback n)
  }

$(makeLenses ''CallbackConfig)

defaultCallbackConfig :: Applicative n => RunCallback n -> CallbackConfig n
defaultCallbackConfig runCb = CallbackConfig runCb (\_ _ -> pure defaultLambdaError) (\_ -> pure defaultLambdaError) (\_ -> pure ())

runlift :: MonadIO m => UnliftIO n -> n a -> m a
runlift (UnliftIO runIO) n = liftIO (runIO n)

castLambdaError :: Typeable a => a -> Maybe LambdaError
castLambdaError = cast

castExitCode :: Typeable a => a -> Maybe ExitCode
castExitCode = cast

nonExitCode :: SomeException -> Maybe SomeException
nonExitCode err =
  case castExitCode err of
    Just _ -> Nothing
    Nothing -> Just err

catchNonExitCode :: MonadCatch m => m a -> (SomeException -> m a) -> m a
catchNonExitCode = catchJust nonExitCode

handleGuardError :: MonadIO m => PostLambdaInvokeError m -> UnliftIO n -> InvokeErrorCallback n -> LambdaRequest -> SomeException -> m ()
handleGuardError postErr unio invokeErrCb lamReq err = do
  lamErr <- case castLambdaError err of
    Just lamErr -> pure lamErr
    Nothing -> runlift unio (invokeErrCb lamReq err)
  postErr (_lreqId lamReq) lamErr

guardInvokeError :: (MonadCatch m, MonadIO m) => PostLambdaInvokeError m -> UnliftIO n -> InvokeErrorCallback n -> LambdaRequest -> m () -> m ()
guardInvokeError postErr unio invokeErrCb lamReq body = catchNonExitCode body (handleGuardError postErr unio invokeErrCb lamReq)

pollAndRespond :: (MonadCatch m, MonadIO m) => LambdaClient m -> UnliftIO n -> CallbackConfig n -> m ()
pollAndRespond client unio cbc = do
  let runCb = _cbcRunCallback cbc
      invokeErrCb = _cbcInvokeErrorCallback cbc
      postErr = _lcPostLambdaInvokeError client
      postRep = _lcPostLambdaResponse client
  lamReq <- _lcGetLambdaRequest client
  guardInvokeError postErr unio invokeErrCb lamReq $ do
    lamRepBody <- runlift unio (runCb lamReq)
    let lamReqId = _lreqId lamReq
    postRep lamReqId lamRepBody

pollLoop :: (MonadCatch m, MonadIO m) => LambdaClient m -> UnliftIO n -> CallbackConfig n -> m ()
pollLoop client unio cbc =
  let uncaughtErrCb = _cbcUncaughtErrorCallback cbc
  in forever (catchNonExitCode (pollAndRespond client unio cbc) (runlift unio . uncaughtErrCb))

handleInitError :: MonadIO m => PostLambdaInitError m -> UnliftIO n -> InitErrorCallback n -> SomeException -> m ()
handleInitError postErr unio initErrCb err = do
  lamErr <- case castLambdaError err of
    Just lamErr -> pure lamErr
    Nothing -> runlift unio (initErrCb err)
  postErr lamErr

data LambdaVars = LambdaVars
  { _lvLogGroupName :: !Text
  , _lvLogStreamName :: !Text
  , _lvFunctionVersion :: !Text
  , _lvFunctionName :: !Text
  , _lvTaskRoot :: !Text
  , _lvApiEndpoint :: !Text
  , _lvFunctionMemory :: !Text
  , _lvHandlerName :: !Text
  }

$(makeLenses ''LambdaVars)

data LambdaEnv = LambdaEnv
  { _leManager :: !Http.Manager
  , _leVars :: !LambdaVars
  }

$(makeLenses ''LambdaEnv)

getEnvText :: (MonadThrow m, MonadIO m) => Text -> m Text
getEnvText = fmap Text.pack . getEnv . Text.unpack

readLambdaVars :: (MonadThrow m, MonadIO m) => m LambdaVars
readLambdaVars = do
  logGroupName <- getEnvText "AWS_LAMBDA_LOG_GROUP_NAME"
  logStreamName <- getEnvText "AWS_LAMBDA_LOG_STREAM_NAME"
  functionVersion <- getEnvText "AWS_LAMBDA_FUNCTION_VERSION"
  functionName <- getEnvText "AWS_LAMBDA_FUNCTION_NAME"
  taskRoot <- getEnvText "LAMBDA_TASK_ROOT"
  apiEndpoint <- getEnvText "AWS_LAMBDA_RUNTIME_API"
  functionMemory <- getEnvText "AWS_LAMBDA_FUNCTION_MEMORY_SIZE"
  handlerName <- getEnvText "_HANDLER"
  pure LambdaVars
    { _lvLogGroupName = logGroupName
    , _lvLogStreamName = logStreamName
    , _lvFunctionVersion = functionVersion
    , _lvFunctionName = functionName
    , _lvTaskRoot = taskRoot
    , _lvApiEndpoint = apiEndpoint
    , _lvFunctionMemory = functionMemory
    , _lvHandlerName = handlerName
    }

newLambdaEnv :: (MonadThrow m, MonadIO m) => m LambdaEnv
newLambdaEnv = do
  manager <- liftIO (Http.newManager httpManagerSettings)
  vars <- readLambdaVars
  pure (LambdaEnv manager vars)

class HasLambdaEnv env where
  lambdaEnvL :: Lens' env LambdaEnv

instance HasLambdaEnv LambdaEnv where
  lambdaEnvL = id

type MonadLambdaImpl env m = (MonadIO m, MonadReader env m, HasLambdaEnv env)

getLambdaRequestImpl :: MonadLambdaImpl env m => GetLambdaRequest m
getLambdaRequestImpl = undefined

postLambdaInitErrorImpl :: MonadLambdaImpl env m => PostLambdaInitError m
postLambdaInitErrorImpl = undefined

postLambdaInvokeErrorImpl :: MonadLambdaImpl env m => PostLambdaInvokeError m
postLambdaInvokeErrorImpl = undefined

postLambdaResponseImpl :: MonadLambdaImpl env m => PostLambdaResponse m
postLambdaResponseImpl = undefined

lambdaClientImpl :: MonadLambdaImpl env m => LambdaClient m
lambdaClientImpl = LambdaClient
  { _lcGetLambdaRequest = getLambdaRequestImpl
  , _lcPostLambdaInitError = postLambdaInitErrorImpl
  , _lcPostLambdaInvokeError = postLambdaInvokeErrorImpl
  , _lcPostLambdaResponse = postLambdaResponseImpl
  }

httpManagerSettings :: Http.ManagerSettings
httpManagerSettings =
  -- We set the timeout to none, as AWS Lambda freezes the containers.
  Http.defaultManagerSettings
  { Http.managerResponseTimeout = Http.responseTimeoutNone
  }

mkMain :: UnliftIO n -> CallbackConfig n -> IO ()
mkMain unio cbc = do
  lambdaEnv <- newLambdaEnv
  runRIO lambdaEnv $ do
    pollLoop lambdaClientImpl unio cbc
