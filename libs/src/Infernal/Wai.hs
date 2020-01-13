-- | DOCME
module Infernal.Wai
  ( adaptApplication
  , adaptRequest
  , adaptResponse
  , applicationCallback
  , runSimpleLambda
  ) where

import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Data.Binary.Builder (Builder, toLazyByteString)
import qualified Data.ByteString as ByteString
import Data.ByteString.Lazy (toStrict)
import Data.IORef (modifyIORef, newIORef, readIORef)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8)
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
  , pathInfo = Text.split (=='/') (decodeUtf8 (_agprqPath proxyReq))
  , queryString = _agprqQueryStringParameters proxyReq
  , requestHeaders = _agprqHeaders proxyReq
  , requestBody = maybe empty pure (_agprqBody proxyReq)
  }

consumeStream :: StreamingBody -> IO Builder
consumeStream sb = do
  m <- newEmptyMVar
  r <- newIORef mempty
  sb (modifyIORef r . flip mappend) (readIORef r >>= putMVar m)
  takeMVar m

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
  _ <- liftIO (app (adaptRequest proxyReq) (\res -> adaptResponse res >>= putMVar v >> pure ResponseReceived))
  liftIO (takeMVar v)

-- | DOCME
applicationCallback :: (MonadThrow n, MonadIO n) => Application -> RunCallback n
applicationCallback app lamReq = do
  proxyReq <- decodeRequest lamReq
  proxyRep <- adaptApplication app proxyReq
  pure (encodeResponse proxyRep)

-- | DOCME
runSimpleWaiLambda :: Application -> IO ()
runSimpleWaiLambda = runSimpleLambda . applicationCallback
