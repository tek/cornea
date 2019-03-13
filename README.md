# Intro

Classes for accessing and mutating nested data types with corresponding adapter classes for `MonadState` and `MonadError`.

# Example

For `MonadError`:

```haskell
{-# LANGUAGE TemplateHaskell #-}

import Control.Monad.DeepError (MonadDeepError(throwHoist))
import Control.Monad.Trans.Except (runExceptT)
import Data.DeepPrisms (deepPrisms)

newtype Error = Error String

newtype Inner = Inner Error
deepPrisms ''Inner

data Mid = Mid Inner
deepPrisms ''Mid

newtype Outer = Outer Mid
deepPrisms ''Outer

throwDeep :: MonadDeepError e Inner m => m ()
throwDeep = throwHoist (Inner (Error "boom"))

main :: IO (Either Outer ())
main = runExceptT throwDeep
```

In `main`, `MonadError Outer IO` and `DeepPrisms Outer Inner` is summoned.

Analogously for `MonadState`:

```haskell
{-# LANGUAGE TemplateHaskell #-}

import Control.Monad.DeepState (MonadDeepState(get, gets, put))
import Control.Monad.Trans.State (execStateT)
import Data.DeepLenses (deepLenses)

newtype S = S Int

newtype Inner = Inner { _innerS :: S }
deepLenses ''Inner

data Mid = Mid { _midInner :: Inner }
deepLenses ''Mid

newtype Outer = Outer { _outerMid :: Mid }
deepLenses ''Outer

stateDeep :: MonadDeepState s Inner m => m ()
stateDeep = do
  (Inner (S a)) <- get
  b <- gets $ \(Inner (S b)) -> b
  put (Inner (S (a + b + 3)))

main :: IO Outer
main = do
  execStateT stateDeep (Outer (Mid (Inner (S 5))))
```
