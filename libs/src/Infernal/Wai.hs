{-|
Functions to let you wrap WAI Applications and use them to serve API Gateway requests.
See 'runSimpleWaiLambda' for a simple entrypoint.
-}
module Infernal.Wai
  ( adaptApplication
  , adaptRequest
  , adaptResponse
  , applicationCallback
  , runSimpleWaiLambda
  ) where

import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Binary.Builder (Builder, toLazyByteString)
import qualified Data.ByteString as ByteString
import Data.ByteString.Lazy (toStrict)
import Data.IORef (modifyIORef', newIORef, readIORef)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8)
import Infernal (RunCallback, decodeRequest, encodeResponse, runSimpleLambda)
import Infernal.Events.APIGateway (APIGatewayProxyRequest (..), APIGatewayProxyResponse (..))
import LittleLogger (WithSimpleLog, logDebug)
import Network.Wai (Application, Response, StreamingBody, defaultRequest, responseToStream)
import Network.Wai.Internal (Request (..), ResponseReceived (..))
import Prelude

-- | Turn an 'APIGatewayProxyRequest' into a WAI 'Request'. (Not all fields will be present!)
adaptRequest :: APIGatewayProxyRequest -> Request
adaptRequest proxyReq =
  defaultRequest
  { requestMethod = _agprqHttpMethod proxyReq
  , rawPathInfo = _agprqPath proxyReq
  , pathInfo = dropWhile Text.null (Text.split (=='/') (decodeUtf8 (_agprqPath proxyReq)))
  , queryString = _agprqQueryStringParameters proxyReq
  , requestHeaders = _agprqHeaders proxyReq
  , requestBody = maybe mempty pure (_agprqBody proxyReq)
  }

consumeStream :: StreamingBody -> IO Builder
consumeStream sb = do
  r <- newIORef mempty
  sb (modifyIORef' r . flip mappend) (pure ())
  readIORef r

-- | Turn a WAI 'Response' into an 'APIGatewayProxyResponse', materializing the whole response body.
adaptResponse :: MonadIO n => Response -> n APIGatewayProxyResponse
adaptResponse rep = do
  let (repStatus, repHeaders, repBodyAction) = responseToStream rep
  bodyBuilder <- liftIO (repBodyAction consumeStream)
  let body = toStrict (toLazyByteString bodyBuilder)
  pure APIGatewayProxyResponse
    { _agprsStatusCode = fromEnum repStatus
    , _agprsHeaders = repHeaders
    , _agprsBody = if ByteString.null body then Nothing else Just body
    }

-- | Adapt a WAI 'Application' into a function that handles API Gateway proxy requests.
adaptApplication :: MonadIO n => Application -> APIGatewayProxyRequest -> n APIGatewayProxyResponse
adaptApplication app proxyReq = do
  v <- liftIO newEmptyMVar
  let req = adaptRequest proxyReq
  _ <- liftIO $ app req $ \res -> do
    proxyRes <- adaptResponse res
    putMVar v proxyRes
    pure ResponseReceived
  liftIO (takeMVar v)

-- | Adapt a WAI 'Application' into a 'RunCallback' to handle API Gateway proxy requests encoded as Lambda requests.
applicationCallback :: (MonadThrow n, WithSimpleLog env n) => Application -> RunCallback n
applicationCallback app lamReq = do
  proxyReq <- decodeRequest lamReq
  logDebug ("Servicing proxy request " <> decodeUtf8 (_agprqHttpMethod proxyReq) <> " " <> decodeUtf8 (_agprqPath proxyReq))
  proxyRep <- adaptApplication app proxyReq
  logDebug ("Responding with status " <> Text.pack (show (_agprsStatusCode proxyRep)))
  let lamRep = encodeResponse proxyRep
  pure lamRep

-- | A simple entrypoint to run your WAI 'Application' in a Lambda function. (See 'runSimpleLambda' for more information on these entrypoints.)
--   You can configure API Gateway to send proxied HTTP requests as JSON to your Lambda (as 'APIGatewayProxyRequest') and have your WAI Application
--   service them with this entrypoint. (Correct API Gateway configuration is pretty tricky, so consult all the documentation available to figure it out.)
runSimpleWaiLambda :: Application -> IO ()
runSimpleWaiLambda = runSimpleLambda . applicationCallback
