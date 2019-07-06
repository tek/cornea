{-# LANGUAGE NoImplicitPrelude #-}

module Cornea (
  module Control.Monad.DeepError,
  module Control.Monad.DeepState,
  module Data.DeepLenses,
  module Data.DeepPrisms,
  modify,
) where

import Control.Monad.DeepError
import qualified Control.Monad.DeepState as DeepState (modify)
import Control.Monad.DeepState hiding (modify)
import Data.DeepLenses (deepLenses)
import Data.DeepPrisms (deepPrisms)

modify ::
  ∀ s' s m .
  MonadDeepState s s' m =>
  (s' -> s') ->
  m ()
modify =
  DeepState.modify
