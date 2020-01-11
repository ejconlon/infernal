# infernal

The Infernal Machine - An AWS Lambda Custom Runtime for Haskell

Special thanks to Nikita Tchayka and Theam for [aws-lambda-haskell-runtime](http://hackage.haskell.org/package/aws-lambda-haskell-runtime).
Between reading their implementation and the [official documentation](https://docs.aws.amazon.com/lambda/latest/dg/runtimes-custom.html#runtimes-custom-build)
it was pretty easy to get this working. `aws-lambda-haskell-runtime` is a great library but I had trouble combining the `TemplateHaskell` entrypoint
with some custom startup logic.

The `infernal` subdir has the library, and `infernal-demo` has a small example application. They are separate `stack` projects because the demo
needs to be built with Docker on OSX. See the `Brewfile` for test deps on OSX. Basically, if you have `stack`, `docker`, `upx`, and `aws-sam-cli`
installed, you can run `make integration-test` to check a few cases.

## TODO

* Get all deps into stackage (heart-core, heart-app)
* Cleanup exports and add docs
* Set up CI
* Provide a stack template
* Add APIGateway and other standard events
* Add wrapper to serve existing WAI Applications with APIGateway
