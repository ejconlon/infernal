module Infernal
  ( CallbackConfig (..)
  , InitErrorCallback
  , InvokeErrorCallback
  , LambdaError (..)
  , LambdaRequestId (..)
  , LambdaRequest (..)
  , LambdaVars (..)
  , RunCallback
  , UncaughtErrorCallback
  , defaultCallbackConfig
  , defaultLambdaError
  , defaultInitErrorCallback
  , mkMain
  , mkSimpleMain
  ) where

import Control.Monad (forever, void)
import Data.Aeson (encode, pairs, (.=))
import qualified Data.ByteString.Lazy as LBS
import qualified Data.CaseInsensitive as CI
import Data.Maybe (isJust)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Heart.App.SuperPrelude
import qualified Network.HTTP.Client as HC
import qualified Network.HTTP.Types as HT
import System.Exit (ExitCode)
import System.IO (BufferMode (LineBuffering), hSetBuffering, stderr, stdout)
import Text.Read (readMaybe)
import UnliftIO.Environment (getEnv)

-- TODO This should go in heart-core, along with catch functions
unliftRIO :: MonadIO m => env -> m (UnliftIO (RIO env))
unliftRIO env = liftIO (runRIO env askUnliftIO)

unliftPure :: UnliftIO IO
unliftPure = UnliftIO id

newtype LambdaRequestId = LambdaRequestId
  { _unLambdaRequestId :: Text
  } deriving newtype (Eq, Show, Ord, IsString, Hashable)

$(makeLenses ''LambdaRequestId)

data LambdaRequest = LambdaRequest
  { _lreqId :: !LambdaRequestId
  , _lreqTraceId :: !Text
  , _lreqFunctionArn :: !Text
  , _lreqDeadlineMs :: !Int
  , _lreqBody :: !LBS.ByteString
  } deriving stock (Eq, Show)

$(makeLenses ''LambdaRequest)

data LambdaError = LambdaError
  { _lerrErrorType :: !Text
  , _lerrErrorMessage :: !Text
  } deriving stock (Eq, Show, Typeable, Generic)

$(makeLenses ''LambdaError)

instance Exception LambdaError

instance ToJSON LambdaError where
  toEncoding (LambdaError ty msg) = pairs ("errorType" .= ty <> "errorMessage" .= msg)

defaultLambdaError :: LambdaError
defaultLambdaError = LambdaError "InternalError" "No information is available."

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

class HasLambdaEnv env where
  lambdaEnvL :: Lens' env LambdaEnv

instance HasLambdaEnv LambdaEnv where
  lambdaEnvL = id

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

unliftInto :: MonadIO m => UnliftIO n -> n a -> m a
unliftInto (UnliftIO runIO) n = liftIO (runIO n)

castLambdaError :: SomeException -> Maybe LambdaError
castLambdaError = fromException

castExitCode :: SomeException -> Maybe ExitCode
castExitCode = fromException

catchRethrowWhen :: MonadCatch m => (SomeException -> Bool) -> m a -> (SomeException -> m a) -> m a
catchRethrowWhen predicate act handler = catch act (\err -> handler err >>= if predicate err then const (throwM err) else pure)

catchRethrow :: MonadCatch m => m a -> (SomeException -> m ()) -> m a
catchRethrow act handler = catch act (\err -> handler err >> throwM err)

catchRethrowExitCode :: MonadCatch m => m a -> (SomeException -> m a) -> m a
catchRethrowExitCode = catchRethrowWhen (isJust . castExitCode)

handleInvokeError :: WithSimpleLog env m => PostLambdaInvokeError m -> UnliftIO n -> InvokeErrorCallback n -> LambdaRequest -> SomeException -> m ()
handleInvokeError postErr unio invokeErrCb lamReq err = do
  let lamIdText = _unLambdaRequestId (_lreqId lamReq)
  logError ("Caught invocation error for request id " <> lamIdText <> ":")
  logException err
  lamErr <- case castLambdaError err of
    Just lamErr -> do
      logError ("Posting original invocation error for request id " <> lamIdText)
      pure lamErr
    Nothing -> do
      lamErr <- unliftInto unio (invokeErrCb lamReq err)
      logError ("Posting new invocation error for request id " <> lamIdText <> ":")
      logException lamErr
      pure lamErr
  postErr (_lreqId lamReq) lamErr

guardInvokeError :: (MonadCatch m, WithSimpleLog env m) => PostLambdaInvokeError m -> UnliftIO n -> InvokeErrorCallback n -> LambdaRequest -> m () -> m ()
guardInvokeError postErr unio invokeErrCb lamReq body = catchRethrowExitCode body (handleInvokeError postErr unio invokeErrCb lamReq)

missedDeadlineError :: Int -> LambdaError
missedDeadlineError deadlineMs = LambdaError "MissedDeadlineError" ("Missed adjusted deadline of " <> Text.pack (show deadlineMs) <> "ms.")

pollAndRespond :: (MonadCatch m, WithSimpleLog env m) => LambdaClient m -> UnliftIO n -> CallbackConfig n -> m ()
pollAndRespond client unio cbc = do
  let runCb = _cbcRunCallback cbc
      invokeErrCb = _cbcInvokeErrorCallback cbc
      postErr = _lcPostLambdaInvokeError client
      postRep = _lcPostLambdaResponse client
  logDebug "Polling for request"
  lamReq <- _lcGetLambdaRequest client
  let lamReqId = _lreqId lamReq
      lamIdText = _unLambdaRequestId lamReqId
  logDebug ("Servicing request id " <> lamIdText)
  guardInvokeError postErr unio invokeErrCb lamReq $ do
    lamRepBody <- unliftInto unio (runCb lamReq)
    logDebug ("Posting response to request id " <> lamIdText)
    postRep lamReqId lamRepBody
    logDebug ("Finished request id " <> lamIdText)

pollLoop :: (MonadCatch m, WithSimpleLog env m) => LambdaClient m -> UnliftIO n -> CallbackConfig n -> m ()
pollLoop client unio cbc =
  let uncaughtErrCb = _cbcUncaughtErrorCallback cbc
  in forever $ catchRethrowExitCode (pollAndRespond client unio cbc) $ \err -> do
    logError "Handling uncaught error:"
    logException err
    unliftInto unio (uncaughtErrCb err)

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

missingHeaderError :: Text -> LambdaError
missingHeaderError name = LambdaError "MissingHeaderError" ("Missing header " <> name <> " in next invocation request.")

invalidDeadlineError :: Text -> LambdaError
invalidDeadlineError deadlineMsText = LambdaError "InvalidDeadlineError" ("Invalid value for deadline: " <> deadlineMsText)

lookupHeader :: MonadThrow m => Text -> HT.ResponseHeaders -> m Text
lookupHeader name headers =
  case lookup (CI.mk (encodeUtf8 name)) headers of
    Nothing -> throwM (missingHeaderError name)
    Just value -> pure (decodeUtf8 value)

parseLambdaRequest :: MonadThrow m => HT.ResponseHeaders -> LBS.ByteString -> m LambdaRequest
parseLambdaRequest headers body = do
  reqId <- lookupHeader "Lambda-Runtime-Aws-Request-Id" headers
  traceId <- lookupHeader "Lambda-Runtime-Trace-Id" headers
  functionArn <- lookupHeader "Lambda-Runtime-Invoked-Function-Arn" headers
  deadlineMsText <- lookupHeader "Lambda-Runtime-Deadline-Ms" headers
  deadlineMs <- case readMaybe (Text.unpack deadlineMsText) of
    Just value -> pure value
    Nothing -> throwM (invalidDeadlineError deadlineMsText)
  pure $ LambdaRequest
    { _lreqId = LambdaRequestId reqId
    , _lreqTraceId = traceId
    , _lreqFunctionArn = functionArn
    , _lreqDeadlineMs = deadlineMs
    , _lreqBody = body
    }

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

mkMain :: (MonadCatch m, WithSimpleLog env m) => UnliftIO n -> InitErrorCallback n -> (LambdaVars -> n (CallbackConfig n)) -> m ()
mkMain unio initErrCb cbcInit = do
  logDebug "Initializing lambda environment"
  lambdaEnv <- newLambdaEnv
  runRIO lambdaEnv $ do
    let client = lambdaClientImpl
        lamVars = _leVars lambdaEnv
    logDebug "Initializing callbacks"
    cbc <- catchRethrow (unliftInto unio (cbcInit lamVars)) $ \err -> do
      logError "Caught initialization error:"
      logException err
      lamErr <- unliftInto unio (initErrCb err)
      logError "Posting mapped initialization error:"
      logException lamErr
      _lcPostLambdaInitError client lamErr
    logDebug "Starting poll loop"
    pollLoop client unio cbc

mkSimpleMain :: RunCallback (RIO App) -> IO ()
mkSimpleMain cb = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  app <- newApp
  unio <- unliftRIO app
  runRIO app (mkMain unio defaultInitErrorCallback (const (pure (defaultCallbackConfig cb))))
