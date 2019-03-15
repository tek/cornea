module Control.Monad.DeepState where

import qualified Control.Lens as Lens (set, view)
import Control.Monad.State.Class (MonadState)
import qualified Control.Monad.State.Class as MS (MonadState(get), modify)

import Data.DeepLenses (DeepLenses(deepLens))

class (MonadState s m, DeepLenses s s') => MonadDeepState s s' m where
  get :: m s'
  put :: s' -> m ()
  stateM :: (s' -> m (a, s')) -> m a

instance (MonadState s m, DeepLenses s s') => MonadDeepState s s' m where
  get = Lens.view deepLens <$> MS.get
  put = MS.modify . Lens.set deepLens
  stateM f = do
    s' <- get
    ~(a, s'') <- f s'
    put s''
    return a

state :: MonadDeepState s s' m => (s' -> (a, s')) -> m a
state f = stateM (pure . f)

gets :: MonadDeepState s s' m => (s' -> a) -> m a
gets =
  (<$> get)

modifyM :: MonadDeepState s s' m => (s' -> m s') -> m ()
modifyM f =
  stateM (fmap ((),) . f)

modify :: MonadDeepState s s' m => (s' -> s') -> m ()
modify f =
  modifyM (pure . f)
