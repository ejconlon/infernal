module Main where

import Infernal.Wai (runSimpleWaiLambda)
import Prelude
import Web.Scotty (ScottyM, get, html, param, scottyApp)

server :: ScottyM ()
server =
  get "/:word" $ do
    beam <- param "word"
    html (mconcat ["<h1>Scotty, ", beam, " me up!</h1>"])

main :: IO ()
main = do
  app <- scottyApp server
  runSimpleWaiLambda app
