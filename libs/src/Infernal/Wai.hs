-- | DOCME
module Infernal.Wai
  ( adaptApplication
  , adaptRequest
  , adaptResponse
  , applicationCallback
  , runSimpleWaiLambda
  ) where

import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Data.Binary.Builder (Builder, toLazyByteString)
import qualified Data.ByteString as ByteString
import Data.ByteString.Lazy (toStrict)
import Data.IORef (modifyIORef', newIORef, readIORef)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8)
import Heart.App.Logging (WithSimpleLog, logDebug)
import Heart.Core.Prelude
import Infernal (RunCallback, decodeRequest, encodeResponse, runSimpleLambda)
import Infernal.Events.APIGateway (APIGatewayProxyRequest (..), APIGatewayProxyResponse (..))
import Network.Wai (Application, Response, StreamingBody, defaultRequest, responseToStream)
import Network.Wai.Internal (Request (..), ResponseReceived (..))

-- | DOCME
adaptRequest :: APIGatewayProxyRequest -> Request
adaptRequest proxyReq =
  defaultRequest
  { requestMethod = _agprqHttpMethod proxyReq
  , rawPathInfo = _agprqPath proxyReq
  , pathInfo = dropWhile Text.null (Text.split (=='/') (decodeUtf8 (_agprqPath proxyReq)))
  , queryString = _agprqQueryStringParameters proxyReq
  , requestHeaders = _agprqHeaders proxyReq
  , requestBody = maybe empty pure (_agprqBody proxyReq)
  }

consumeStream :: StreamingBody -> IO Builder
consumeStream sb = do
  r <- newIORef mempty
  sb (modifyIORef' r . flip mappend) (pure ())
  readIORef r

-- | DOCME
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

-- | DOCME
adaptApplication :: MonadIO n => Application -> APIGatewayProxyRequest -> n APIGatewayProxyResponse
adaptApplication app proxyReq = do
  v <- liftIO newEmptyMVar
  let req = adaptRequest proxyReq
  _ <- liftIO $ app req $ \res -> do
    proxyRes <- adaptResponse res
    putMVar v proxyRes
    pure ResponseReceived
  liftIO (takeMVar v)

-- | DOCME
applicationCallback :: (MonadThrow n, WithSimpleLog env n) => Application -> RunCallback n
applicationCallback app lamReq = do
  proxyReq <- decodeRequest lamReq
  logDebug ("Servicing proxy request " <> decodeUtf8 (_agprqHttpMethod proxyReq) <> " " <> decodeUtf8 (_agprqPath proxyReq))
  proxyRep <- adaptApplication app proxyReq
  logDebug ("Responding with status " <> Text.pack (show (_agprsStatusCode proxyRep)))
  let lamRep = encodeResponse proxyRep
  pure lamRep

-- | DOCME
runSimpleWaiLambda :: Application -> IO ()
runSimpleWaiLambda = runSimpleLambda . applicationCallback
