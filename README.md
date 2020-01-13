# infernal

The Infernal Machine - An AWS Lambda Custom Runtime for Haskell

## How to write your own Lambda

* See `Makefile`, `stack.yaml`, and `package.yaml` in `demo` for the right options to build your program
  * statically-linked
  * in a Docker container
  * zipped the right way
  * with some deps temporarily not in Stackage
* Import `Infernal` in your `Main` module and use `runSimpleLambda` to run your handler
* See `sam-test` and root `Makefile` if you want to use `aws-sam-cli` to test your program locally
* Deploy to AWS with your tool of choice (Serverless, Terraform + awscli, AWS console, etc)

## Development

The `libs` subdir has the library, and `demo` has a small example application. They are separate `stack` projects because the demo
needs to be built with Docker on OSX. See the `Brewfile` for test deps on OSX. Basically, if you have `stack`, `docker`, and `aws-sam-cli`
installed, you can run `make integration-test` to check a few cases.

## Prior work

Thanks to Nikita Tchayka and Theam for [aws-lambda-haskell-runtime](http://hackage.haskell.org/package/aws-lambda-haskell-runtime).
Between reading their implementation and the [official documentation](https://docs.aws.amazon.com/lambda/latest/dg/runtimes-custom.html#runtimes-custom-build)
it was pretty easy to get this working. `aws-lambda-haskell-runtime` is a great library but I had trouble combining the `TemplateHaskell` entrypoint
with some custom startup logic.

Also thanks to `akotlyarov` and Seek for [serverless-haskell](https://hackage.haskell.org/package/serverless-haskell). Their API Gateway definitions are
very useful, and I am grateful to not have to hand-roll JSON codecs. I did not introduce the library as a dependency because I wanted to simplify the
definitions a bit and also avoid multiple `amazonka` dependencies not available in Stackage.

You can find copies of both licenses in the `licenses` directory.

## TODO

* Get all deps into stackage (heart-core, heart-app)
* Set up CI
* Provide a stack template
* Add APIGateway and other standard events
* Add wrapper to serve existing WAI Applications with APIGateway
