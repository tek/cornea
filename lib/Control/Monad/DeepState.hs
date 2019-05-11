{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.DeepState where

import Control.Lens (Lens')
import qualified Control.Lens as Lens (mapMOf, over, set, view, views)
import Control.Monad.State.Class (MonadState)
import qualified Control.Monad.State.Class as MS (MonadState(get), modify)
import Control.Monad.Trans.Class (MonadTrans(lift))
import qualified Control.Monad.Trans.State.Lazy as Lazy (StateT)
import Control.Monad.Trans.State.Strict (StateT)
import Data.Either.Combinators (rightToMaybe, swapEither)
import Data.Functor (void)

import Data.DeepLenses (DeepLenses(deepLens))

class Monad m => MonadDeepState (s :: *) (s' :: *) (m :: * -> *) | m -> s where
  get :: m s'
  put :: s' -> m ()
  state :: (s' -> (a, s')) -> m a
  state f = do
    ~(a, s'') <- f <$> get
    a <$ put s''
  modifyM' :: (s' -> m s') -> m s'
  modifyM' f = do
    s' <- f =<< get
    s' <$ put s'
  modify :: (s' -> s') -> m ()
  modify f =
    modifyM (pure . f)

instance {-# OVERLAPPING #-} (Monad m, DeepLenses s s') => MonadDeepState s s' (Lazy.StateT s m) where
  get = Lens.view deepLens <$> MS.get
  put = MS.modify . Lens.set deepLens

instance {-# OVERLAPPING #-} (Monad m, DeepLenses s s') => MonadDeepState s s' (StateT s m) where
  get = Lens.view deepLens <$> MS.get
  put = MS.modify . Lens.set deepLens

instance {-# OVERLAPPABLE #-} (Monad (t m), MonadTrans t, MonadDeepState s s' m) => MonadDeepState s s' (t m) where
  get = lift get
  put = lift . put
  state = lift . state

gets ::
  ∀ s' s m a .
  MonadDeepState s s' m =>
  (s' -> a) ->
  m a
gets =
  (<$> get)

modifyM ::
  MonadDeepState s s' m =>
  (s' -> m s') ->
  m ()
modifyM =
  void . modifyM'

modifyL :: ∀ s' s a m. MonadDeepState s s' m => Lens' s' a -> (a -> a) -> m ()
modifyL lens f =
  modify $ Lens.over lens f

modifyML :: ∀ s' s a m. MonadDeepState s s' m => Lens' s' a -> (a -> m a) -> m ()
modifyML lens f =
  modifyM $ Lens.mapMOf lens f

getL :: ∀ s' s m a. MonadDeepState s s' m => Lens' s' a -> m a
getL =
  gets . Lens.view

getsL :: ∀ s' s m a b. MonadDeepState s s' m => Lens' s' a -> (a -> b) -> m b
getsL lens =
  gets . Lens.views lens

setL :: ∀ s' s m a. MonadDeepState s s' m => Lens' s' a -> a -> m ()
setL lens =
  modify . Lens.set lens
