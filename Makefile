integration-test:
	cd demo && make integration-test

update-licenses:
	wget -O licenses/LICENSE.aws-lambda-haskell-runtime https://raw.githubusercontent.com/theam/aws-lambda-haskell-runtime/master/LICENSE
	wget -O licenses/LICENSE.serverless-haskell https://raw.githubusercontent.com/seek-oss/serverless-haskell/master/LICENSE
