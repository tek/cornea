module Control.Monad.DeepReader where

import Control.Lens (over)
import qualified Control.Lens as Lens (view)
import qualified Control.Monad.Reader.Class as MR (MonadReader(ask))
import Control.Monad.Trans.Reader (ReaderT)

import Data.DeepLenses (DeepLenses(deepLens))

class Monad m => MonadDeepReader (r :: *) (r' :: *) (m :: * -> *) | m -> r where
  ask :: m r'
  local :: (r' -> r') -> m a -> m a
  asks :: (r' -> a) -> m a

instance (Monad m, DeepLenses r r') => MonadDeepReader r r' (ReaderT r m) where
  ask =
    Lens.view deepLens <$> MR.ask
  local f m =
    ReaderT $ runReaderT m . over deepLens f
  asks =
    (<$> ask)
