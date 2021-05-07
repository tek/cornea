{-# language NoImplicitPrelude #-}

module Cornea.Prelude (
  module Relude,
  undefined,
) where

import GHC.Err (undefined)
import Relude hiding (Type, ask, asks, get, gets, hoistEither, local, modify, put, state, undefined)
