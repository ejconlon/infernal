{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Infernal.Import where

import Control.Monad (forever, void)
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe (isJust)
import qualified Data.Text as Text
import Infernal.Prelude
import qualified Network.HTTP.Client as HC
import qualified Network.HTTP.Types as HT
import System.Exit (ExitCode)
import UnliftIO.Environment (getEnv)

newtype LambdaRequestId = LambdaRequestId
  { _unLambdaRequestId :: Text
  } deriving newtype (Eq, Show, Ord, IsString, Hashable)

$(makeLenses ''LambdaRequestId)

data LambdaRequest = LambdaRequest
  { _lreqId :: !LambdaRequestId
  , _lreqBody :: !LBS.ByteString
  } deriving stock (Eq, Show, Generic)

$(makeLenses ''LambdaRequest)

instance FromJSON LambdaRequest where
  parseJSON = undefined

data LambdaError = LambdaError
  { _lerrType :: !Text
  , _lerrMessage :: !Text
  } deriving stock (Eq, Show, Typeable, Generic)
    deriving ToJSON via (AesonRecord LambdaError)

$(makeLenses ''LambdaError)

instance Exception LambdaError

defaultLambdaError :: LambdaError
defaultLambdaError = LambdaError "InternalError" "No information is available."

type GetLambdaRequest m = m LambdaRequest
type PostLambdaInitError m = LambdaError -> m ()
type PostLambdaInvokeError m = LambdaRequestId -> LambdaError -> m ()
type PostLambdaResponse m = LambdaRequestId -> LBS.ByteString -> m ()

data LambdaClient m = LambdaClient
  { _lcGetLambdaRequest :: !(GetLambdaRequest m)
  , _lcPostLambdaInitError :: !(PostLambdaInitError m)
  , _lcPostLambdaInvokeError :: !(PostLambdaInvokeError m)
  , _lcPostLambdaResponse :: !(PostLambdaResponse m)
  }

$(makeLenses ''LambdaClient)

type InitErrorCallback n = SomeException -> n LambdaError

defaultInitErrorCallback :: Applicative n => InitErrorCallback n
defaultInitErrorCallback _ = pure defaultLambdaError

type RunCallback n = LambdaRequest -> n LBS.ByteString
type InvokeErrorCallback n = LambdaRequest -> SomeException -> n LambdaError
type UncaughtErrorCallback n = SomeException -> n ()

data CallbackConfig n = CallbackConfig
  { _cbcRunCallback :: !(RunCallback n)
  , _cbcInvokeErrorCallback :: !(InvokeErrorCallback n)
  , _cbcUncaughtErrorCallback :: !(UncaughtErrorCallback n)
  }

$(makeLenses ''CallbackConfig)

defaultCallbackConfig :: Applicative n => RunCallback n -> CallbackConfig n
defaultCallbackConfig runCb = CallbackConfig runCb (\_ _ -> pure defaultLambdaError) (\_ -> pure ())

runlift :: MonadIO m => UnliftIO n -> n a -> m a
runlift (UnliftIO runIO) n = liftIO (runIO n)

castLambdaError :: Typeable a => a -> Maybe LambdaError
castLambdaError = cast

castExitCode :: Typeable a => a -> Maybe ExitCode
castExitCode = cast

catchRethrowWhen :: MonadCatch m => (SomeException -> Bool) -> m a -> (SomeException -> m a) -> m a
catchRethrowWhen predicate act handler = catch act (\err -> handler err >>= if predicate err then const (throwM err) else pure)

catchRethrow :: MonadCatch m => m a -> (SomeException -> m ()) -> m a
catchRethrow act handler = catch act (\err -> handler err >> throwM err)

catchRethrowExitCode :: MonadCatch m => m a -> (SomeException -> m a) -> m a
catchRethrowExitCode = catchRethrowWhen (isJust . castExitCode)

handleGuardError :: MonadIO m => PostLambdaInvokeError m -> UnliftIO n -> InvokeErrorCallback n -> LambdaRequest -> SomeException -> m ()
handleGuardError postErr unio invokeErrCb lamReq err = do
  lamErr <- case castLambdaError err of
    Just lamErr -> pure lamErr
    Nothing -> runlift unio (invokeErrCb lamReq err)
  postErr (_lreqId lamReq) lamErr

guardInvokeError :: (MonadCatch m, MonadIO m) => PostLambdaInvokeError m -> UnliftIO n -> InvokeErrorCallback n -> LambdaRequest -> m () -> m ()
guardInvokeError postErr unio invokeErrCb lamReq body = catchRethrowExitCode body (handleGuardError postErr unio invokeErrCb lamReq)

pollAndRespond :: (MonadCatch m, MonadIO m) => LambdaClient m -> UnliftIO n -> CallbackConfig n -> m ()
pollAndRespond client unio cbc = do
  let runCb = _cbcRunCallback cbc
      invokeErrCb = _cbcInvokeErrorCallback cbc
      postErr = _lcPostLambdaInvokeError client
      postRep = _lcPostLambdaResponse client
  lamReq <- _lcGetLambdaRequest client
  -- TODO bracket to set x-ray trace id from request in env
  guardInvokeError postErr unio invokeErrCb lamReq $ do
    lamRepBody <- runlift unio (runCb lamReq)
    let lamReqId = _lreqId lamReq
    postRep lamReqId lamRepBody

pollLoop :: (MonadCatch m, MonadIO m) => LambdaClient m -> UnliftIO n -> CallbackConfig n -> m ()
pollLoop client unio cbc =
  let uncaughtErrCb = _cbcUncaughtErrorCallback cbc
  in forever (catchRethrowExitCode (pollAndRespond client unio cbc) (runlift unio . uncaughtErrCb))

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
  { _leManager :: !HC.Manager
  , _leVars :: !LambdaVars
  , _leSimpleLog :: !SimpleLogAction
  }

$(makeLenses ''LambdaEnv)

instance HasSimpleLog LambdaEnv where
  simpleLogL = leSimpleLog

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

newLambdaEnv :: (MonadThrow m, WithSimpleLog env m) => m LambdaEnv
newLambdaEnv = do
  manager <- liftIO (HC.newManager httpManagerSettings)
  vars <- readLambdaVars
  simpleLog <- view simpleLogL
  pure (LambdaEnv manager vars simpleLog)

class HasLambdaEnv env where
  lambdaEnvL :: Lens' env LambdaEnv

instance HasLambdaEnv LambdaEnv where
  lambdaEnvL = id

type MonadLambdaImpl env m = (MonadIO m, MonadThrow m, MonadReader env m, HasLambdaEnv env)

askApiEndpoint :: MonadLambdaImpl env m => m Text
askApiEndpoint = do
  lambdaEnv <- view lambdaEnvL
  pure (_lvApiEndpoint (_leVars lambdaEnv))

askManager :: MonadLambdaImpl env m => m HC.Manager
askManager = do
  lambdaEnv <- view lambdaEnvL
  pure (_leManager lambdaEnv)

baseUrl :: MonadLambdaImpl env m => m [Text]
baseUrl = do
  apiEndpoint <- askApiEndpoint
  pure [apiEndpoint, "2018-06-01", "runtime"]

formatUrl :: MonadLambdaImpl env m => [Text] -> m Text
formatUrl suffix = do
  prefix <- baseUrl
  let parts = prefix <> suffix
  pure (Text.intercalate "/" ("http:/" : parts))

initRequest :: MonadLambdaImpl env m => [Text] -> m HC.Request
initRequest suffix = do
  url <- formatUrl suffix
  HC.parseUrlThrow (Text.unpack url)

parseLambdaRequest :: MonadThrow m => HT.ResponseHeaders -> LBS.ByteString -> m LambdaRequest
parseLambdaRequest = undefined

getLambdaRequestImpl :: MonadLambdaImpl env m => GetLambdaRequest m
getLambdaRequestImpl = do
  manager <- askManager
  rawRequest <- initRequest ["invocation", "next"]
  let request = rawRequest { HC.method = "GET" }
  response <- liftIO (HC.httpLbs request manager)
  let headers = HC.responseHeaders response
  let body = HC.responseBody response
  parseLambdaRequest headers body

postRequest :: MonadLambdaImpl env m => [Text] -> LBS.ByteString -> m ()
postRequest suffix body = do
  manager <- askManager
  rawRequest <- initRequest suffix
  let request = rawRequest { HC.method = "POST", HC.requestBody = HC.RequestBodyLBS body }
  void (liftIO (HC.httpNoBody request manager))

postLambdaInitErrorImpl :: MonadLambdaImpl env m => PostLambdaInitError m
postLambdaInitErrorImpl = postRequest ["init", "error"] . encode

postLambdaInvokeErrorImpl :: MonadLambdaImpl env m => PostLambdaInvokeError m
postLambdaInvokeErrorImpl lamReqId = postRequest ["invocation", _unLambdaRequestId lamReqId, "error"] . encode

postLambdaResponseImpl :: MonadLambdaImpl env m => PostLambdaResponse m
postLambdaResponseImpl lamReqId = postRequest ["invocation", _unLambdaRequestId lamReqId, "response"]

lambdaClientImpl :: MonadLambdaImpl env m => LambdaClient m
lambdaClientImpl = LambdaClient
  { _lcGetLambdaRequest = getLambdaRequestImpl
  , _lcPostLambdaInitError = postLambdaInitErrorImpl
  , _lcPostLambdaInvokeError = postLambdaInvokeErrorImpl
  , _lcPostLambdaResponse = postLambdaResponseImpl
  }

httpManagerSettings :: HC.ManagerSettings
httpManagerSettings = HC.defaultManagerSettings { HC.managerResponseTimeout = HC.responseTimeoutNone }

mkMain :: (MonadCatch m, WithSimpleLog env m) => UnliftIO n -> InitErrorCallback n -> n (CallbackConfig n) -> m ()
mkMain unio initErrCb cbcInit = do
  logDebug "Initializing lambda environment"
  lambdaEnv <- newLambdaEnv
  runRIO lambdaEnv $ do
    let client = lambdaClientImpl
    logDebug "Initializing callbacks"
    cbc <- catchRethrow (runlift unio cbcInit) $ \err -> do
      logError "Caught initialization error:"
      logException err
      lamErr <- runlift unio (initErrCb err)
      logError "Posting mapped initialization error:"
      logException lamErr
      _lcPostLambdaInitError client lamErr
    logDebug "Starting poll loop"
    pollLoop client unio cbc
