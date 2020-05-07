{-| A basic app wrapper. -}
module Infernal.Internal.App
  ( App (..)
  , newApp
  ) where

import Infernal.Internal.Logging (HasSimpleLog (..), SimpleLogAction, defaultSimpleLogAction)
import Lens.Micro.TH (makeLenses)

newtype App = App
  { _appLogAction :: SimpleLogAction
  }

$(makeLenses ''App)

instance HasSimpleLog App where
  simpleLogL = appLogAction

newApp :: App
newApp = App defaultSimpleLogAction
