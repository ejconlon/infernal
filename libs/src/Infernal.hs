{-|
The Infernal Machine - An AWS Lambda Custom Runtime for Haskell

See 'runLambda' or 'runSimpleLambda' for entrypoints to build your Lambda function.
-}
module Infernal
  ( CallbackConfig (..)
  , InitErrorCallback
  , InvokeErrorCallback
  , LambdaError (..)
  , LambdaRequestId (..)
  , LambdaRequest (..)
  , LambdaResponse (..)
  , LambdaVars (..)
  , RunCallback
  , UncaughtErrorCallback
  , decodeRequest
  , defaultCallbackConfig
  , defaultLambdaError
  , defaultInitErrorCallback
  , encodeResponse
  , runLambda
  , runSimpleLambda
  ) where

import Control.Monad (forever, void)
import Data.Aeson (eitherDecode, encode, pairs, (.=))
import qualified Data.ByteString.Lazy as LBS
import qualified Data.CaseInsensitive as CI
import Data.Maybe (isJust)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Heart.App.App (App, newApp)
import Heart.App.Logging (HasSimpleLog (..), SimpleLogAction, WithSimpleLog, logDebug, logError, logException)
import Heart.Core.Prelude
import Heart.Core.RIO (RIO, runRIO)
import qualified Network.HTTP.Client as HC
import qualified Network.HTTP.Types as HT
import System.Exit (ExitCode)
import System.IO (BufferMode (LineBuffering), hSetBuffering, stderr, stdout)
import Text.Read (readMaybe)
import UnliftIO.Environment (getEnv)

-- | The UUID associated with a Lambda request.
newtype LambdaRequestId = LambdaRequestId
  { _unLambdaRequestId :: Text
  } deriving newtype (Eq, Show, Ord, IsString, Hashable)

-- | The request parsed from the "next invocation" API (<https://docs.aws.amazon.com/lambda/latest/dg/runtimes-api.html#runtimes-api-next docs>)
data LambdaRequest = LambdaRequest
  { _lreqId :: !LambdaRequestId   -- ^ From the @Lambda-Runtime-Aws-Request-Id@ header
  , _lreqTraceId :: !Text         -- ^ From the @Lambda-Runtime-Trace-Id@ header
  , _lreqFunctionArn :: !Text     -- ^ From the @Lambda-Runtime-Invoked-Function-Arn@ header
  , _lreqDeadlineMs :: !Int       -- ^ From the @Lambda-Runtime-Deadline-Ms@ header, an epoch deadline in ms
  , _lreqBody :: !LBS.ByteString  -- ^ The unparsed request body. Typically you will 'Data.Aeson.decode' this.
  } deriving stock (Eq, Show)

-- | DOCME
newtype LambdaResponse = LambdaResponse
  { _unLambdaResponse :: LBS.ByteString
  } deriving (Eq, Show, Ord, IsString, Hashable)

-- | An error formatted to propagate to AWS (<https://docs.aws.amazon.com/lambda/latest/dg/runtimes-api.html#runtimes-api-invokeerror docs>).
--   Note that this is an 'Exception' so you can throw it to short-circuit processing and report useful information. By default, if you throw
--   anything else a 'defaultLambdaError' will be reported with no useful information.
data LambdaError = LambdaError
  { _lerrErrorType :: !Text     -- ^ The type of error that occurred. In this library is is often @StartCase@-formated.
  , _lerrErrorMessage :: !Text  -- ^ A useful error message
  } deriving stock (Eq, Show, Typeable, Generic)

instance Exception LambdaError

instance ToJSON LambdaError where
  toEncoding (LambdaError ty msg) = pairs ("errorType" .= ty <> "errorMessage" .= msg)

-- | A 'LambdaError' that indicates a vague @InternalError@ to AWS.
defaultLambdaError :: LambdaError
defaultLambdaError = LambdaError "InternalError" "No information is available."

-- | Environment variables set by AWS (<https://docs.aws.amazon.com/lambda/latest/dg/configuration-envvars.html#configuration-envvars-runtime docs>).
--   You may not need to read any of these, but the implementation needs the API endpoint var to handle requests.
data LambdaVars = LambdaVars
  { _lvLogGroupName :: !Text     -- ^ From the @AWS_LAMBDA_LOG_GROUP_NAME@ env var
  , _lvLogStreamName :: !Text    -- ^ From the @AWS_LAMBDA_LOG_STREAM_NAME@ env var
  , _lvFunctionVersion :: !Text  -- ^ From the @AWS_LAMBDA_FUNCTION_VERSION@ env var
  , _lvFunctionName :: !Text     -- ^ From the @AWS_LAMBDA_FUNCTION_NAME@ env var
  , _lvTaskRoot :: !Text         -- ^ From the @LAMBDA_TASK_ROOT@ env var
  , _lvApiEndpoint :: !Text      -- ^ From the @AWS_LAMBDA_RUNTIME_API@ env var
  , _lvFunctionMemory :: !Text   -- ^ From the @AWS_LAMBDA_FUNCTION_MEMORY_SIZE@ env var
  , _lvHandlerName :: !Text      -- ^ From the @_HANDLER@ env var
  }

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
type PostLambdaResponse m = LambdaRequestId -> LambdaResponse -> m ()

data LambdaClient m = LambdaClient
  { _lcGetLambdaRequest :: !(GetLambdaRequest m)
  , _lcPostLambdaInitError :: !(PostLambdaInitError m)
  , _lcPostLambdaInvokeError :: !(PostLambdaInvokeError m)
  , _lcPostLambdaResponse :: !(PostLambdaResponse m)
  }

-- | Error mapper for init errors. The result will be @POST@ed to the init error endpoint (<https://docs.aws.amazon.com/lambda/latest/dg/runtimes-api.html#runtimes-api-initerror docs>).
--   Exceptions of type 'LambdaError' will not trigger this callback, and 'System.Exit.ExitCode' will be rethrown after it executes.
type InitErrorCallback n = SomeException -> n LambdaError

-- | A 'InitErrorCallback' that always returns 'defaultLambdaError'.
defaultInitErrorCallback :: Applicative n => InitErrorCallback n
defaultInitErrorCallback _ = pure defaultLambdaError

-- | The "function" part of your Lambda: takes a request with a JSON-encoded body and returns a JSON-encoded response body. You can throw any 'Exception' and the appropriate error
--   callbacks will process it. Most importantly, 'LambdaError' will propagate a formatted error to AWS, and 'System.Exit.ExitCode' will halt the program. Except for 'System.Exit.ExitCode',
--   throwing exceptions here will not terminate the main loop (see 'runLambda'). Note that the AWS custom runtime loop implemented in this library is single-threaded (as required - we must finish an invocation
--   before fetching the next) but you are free to spawn threads in your callback.
type RunCallback n = LambdaRequest -> n LambdaResponse

-- | Error mapper for invocation errors. The result will be @POST@ed to the invocation error endpoint (<https://docs.aws.amazon.com/lambda/latest/dg/runtimes-api.html#runtimes-api-invokeerror docs>).
--   Exceptions of type 'LambdaError' will not trigger this callback, and 'System.Exit.ExitCode' will be rethrown after it executes.
type InvokeErrorCallback n = LambdaRequest -> SomeException -> n LambdaError

-- | A handler for otherwise uncaught errors (like failures to fetch next invocation). These happen outside context in which we can report them to AWS, so there is no need to return a 'LambdaError'.
--   Exceptions of type 'System.Exit.ExitCode' will be rethrown after this callback executes.
type UncaughtErrorCallback n = SomeException -> n ()

-- | User callbacks to be invoked by the main loop (see 'runLambda').
data CallbackConfig n = CallbackConfig
  { _cbcRunCallback :: !(RunCallback n)                       -- ^ See 'RunCallback'
  , _cbcInvokeErrorCallback :: !(InvokeErrorCallback n)       -- ^ See 'InvokeErrorCallback'
  , _cbcUncaughtErrorCallback :: !(UncaughtErrorCallback n)   -- ^ See 'UncaughtErrorCallback'
  }

-- | A 'CallbackConfig' that returns 'defaultLambdaError' from all error callbacks.
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
postLambdaResponseImpl lamReqId = postRequest ["invocation", _unLambdaRequestId lamReqId, "response"] . _unLambdaResponse

lambdaClientImpl :: MonadLambdaImpl env m => LambdaClient m
lambdaClientImpl = LambdaClient
  { _lcGetLambdaRequest = getLambdaRequestImpl
  , _lcPostLambdaInitError = postLambdaInitErrorImpl
  , _lcPostLambdaInvokeError = postLambdaInvokeErrorImpl
  , _lcPostLambdaResponse = postLambdaResponseImpl
  }

httpManagerSettings :: HC.ManagerSettings
httpManagerSettings = HC.defaultManagerSettings { HC.managerResponseTimeout = HC.responseTimeoutNone }

-- TODO This should go in heart-core, along with catch functions
unliftRIO :: MonadIO m => env -> m (UnliftIO (RIO env))
unliftRIO env = liftIO (runRIO env askUnliftIO)

badRequestError :: Text -> LambdaError
badRequestError reason = LambdaError "BadRequestError" ("Bad request: " <> reason)

-- | DOCME
decodeRequest :: (MonadThrow m, FromJSON a) => LambdaRequest -> m a
decodeRequest = either (throwM . badRequestError . Text.pack) pure . eitherDecode . _lreqBody

-- | DOCME
encodeResponse :: ToJSON a => a -> LambdaResponse
encodeResponse = LambdaResponse . encode

-- | The full-powered entrypoint underlying 'runSimpleLambda' that allows you to use any 'UnliftIO'-capable monad for your callbacks.
--   This runs the main loop of our AWS Lambda Custom Runtime to fetch invocations, process them, and report errors or results.
--   Control will not return from this function, and AWS Lambda will terminate the process at its will.
runLambda ::
  (MonadCatch m, WithSimpleLog env m)
  => UnliftIO n                            -- ^ Runs your monad @n@ in IO (see @MonadUnliftIO@ from @unliftio-core@)
  -> InitErrorCallback n                   -- ^ Error mapper for the callback builder
  -> (LambdaVars -> n (CallbackConfig n))  -- ^ Callback builder. When possible, do init work here so the framework can propagate init errors to AWS.
  -> m ()
runLambda unio initErrCb cbcInit = do
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

-- | A simple entrypoint that delegates to 'runLambda'. Use this as the body of your @main@ function if you want to get a Lambda function up and running quickly.
--   All you need to do is provide a 'RunCallback' that handles JSON-encoded requests and returns JSON-encoded responses (or throws 'LambdaError' exceptions).
--   Your callback has access to a simple logger (try 'logDebug', for example) whose output will be collected by Lambda and published to CloudWatch.
runSimpleLambda :: RunCallback (RIO App) -> IO ()
runSimpleLambda cb = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  app <- newApp
  unio <- unliftRIO app
  runRIO app (runLambda unio defaultInitErrorCallback (const (pure (defaultCallbackConfig cb))))
