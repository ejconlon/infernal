{-|
Definitions for API Gateway proxy objects. Where possible, information that maps directly
to 'Network.HTTP.Types' objects uses that representation. Otherwise a 'Text' representation
is used as 'Data.Aeson' natively provides.

See <https://docs.aws.amazon.com/apigateway/latest/developerguide/set-up-lambda-proxy-integrations.html AWS docs> for
information on these datatypes.

These definitions are taken in part from <https://hackage.haskell.org/package/serverless-haskell serverless-haskell>.
Attribution and license information are present in the README.
-}
module Infernal.Events.APIGateway
  ( APIGatewayProxyRequest (..)
  , APIGatewayProxyResponse (..)
  ) where

import Data.Aeson (object, withObject, (.!=), (.:), (.:?), (.=))
import Data.Bifunctor (bimap)
import Data.ByteString (ByteString)
import qualified Data.CaseInsensitive as CI
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Heart.Core.Prelude
import qualified Network.HTTP.Types as HT

fromAWSQuery :: HashMap Text Text -> HT.Query
fromAWSQuery = fmap toQueryItem . HashMap.toList where
  toQueryItem = bimap encodeUtf8 (\x -> if Text.null x then Nothing else Just (encodeUtf8 x))

toAWSHeaders :: HT.ResponseHeaders -> HashMap Text Text
toAWSHeaders = HashMap.fromList . fmap (bimap (decodeUtf8 . CI.original) decodeUtf8)

fromAWSHeaders :: HashMap Text Text -> HT.RequestHeaders
fromAWSHeaders = fmap toHeader . HashMap.toList where
  toHeader = bimap (CI.mk . encodeUtf8) encodeUtf8

-- | An API Gateway proxy request
data APIGatewayProxyRequest = APIGatewayProxyRequest
  { _agprqResource              :: !Text
  , _agprqPath                  :: !ByteString
  , _agprqHttpMethod            :: !HT.Method
  , _agprqHeaders               :: !HT.RequestHeaders
  , _agprqQueryStringParameters :: !HT.Query
  , _agprqPathParameters        :: !(HashMap Text Text)
  , _agprqStageVariables        :: !(HashMap Text Text)
  , _agprqBody                  :: !(Maybe ByteString)
  } deriving (Eq, Show, Generic)

instance FromJSON APIGatewayProxyRequest where
  parseJSON =
    withObject "APIGatewayProxyRequest" $ \o ->
      APIGatewayProxyRequest
      <$> o .: "resource"
      <*> (encodeUtf8 <$> o .: "path")
      <*> (encodeUtf8 <$> o .: "httpMethod")
      <*> (fmap fromAWSHeaders <$> o .:? "headers") .!= mempty
      <*> (fmap fromAWSQuery <$> o .:? "queryStringParameters") .!= mempty
      <*> o .:? "pathParameters" .!= HashMap.empty
      <*> o .:? "stageVariables" .!= HashMap.empty
      <*> (fmap encodeUtf8 <$> o .:? "body")

-- | An API Gateway proxy response
data APIGatewayProxyResponse = APIGatewayProxyResponse
  { _agprsStatusCode :: !Int
  , _agprsHeaders    :: !HT.ResponseHeaders
  , _agprsBody       :: !(Maybe ByteString)
  } deriving (Eq, Show)

instance ToJSON APIGatewayProxyResponse where
  toJSON rep =
    object
      [ "statusCode" .= _agprsStatusCode rep
      , "headers" .= toAWSHeaders (_agprsHeaders rep)
      , "body" .= fmap decodeUtf8 (_agprsBody rep)
      ]

instance FromJSON APIGatewayProxyResponse where
  parseJSON =
    withObject "APIGatewayProxyResponse" $ \o ->
      APIGatewayProxyResponse
        <$> o .: "statusCode"
        <*> (fromAWSHeaders <$> o .: "headers")
        <*> (fmap encodeUtf8 <$> o .:? "body")
