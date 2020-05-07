-- {-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE FunctionalDependencies #-}
-- {-# LANGUAGE UndecidableInstances #-}

{- |
Most definitions follow the RIO lib: https://hackage.haskell.org/package/rio-0.1.14.0/docs/RIO.html
See LICENSE info in the README.
-}
module Infernal.Internal.RIO
  ( RIO
  , runRIO
  , unliftRIO
  ) where

import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.IO.Unlift (MonadUnliftIO (..), UnliftIO)
import Control.Monad.Reader (MonadReader, ReaderT (..))
import Prelude

newtype RIO env a = RIO { unRIO :: ReaderT env IO a }
  deriving (Functor, Applicative, Monad, MonadReader env, MonadIO, MonadThrow, MonadFail, MonadCatch, MonadMask, MonadUnliftIO)

runRIO :: MonadIO m => env -> RIO env a -> m a
runRIO r m = liftIO (runReaderT (unRIO m) r)

unliftRIO :: MonadIO m => env -> m (UnliftIO (RIO env))
unliftRIO env = liftIO (runRIO env askUnliftIO)
