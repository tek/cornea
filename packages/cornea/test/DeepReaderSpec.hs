module DeepReaderSpec where

import Control.Monad.DeepReader (MonadDeepReader(ask), asks)
import Data.DeepLenses (deepLenses)
import Hedgehog (TestT, (===))

newtype S1 =
  S1 Int
  deriving (Eq, Show)

newtype S2 =
  S2 Int
  deriving (Eq, Show)

newtype S =
  SC { _sS1 :: S1 }
  deriving (Eq, Show)

deepLenses ''S

newtype Bot =
  BotC { _botS :: S }
  deriving (Eq, Show)

deepLenses ''Bot

newtype MiddleOther =
  MiddleOther Int
  deriving (Eq, Show)

data MiddleS =
  MiddleSC {
    _middleBot :: Bot,
    _middleS2 :: S2
    }
  deriving (Eq, Show)

deepLenses ''MiddleS

newtype MainS =
  MainSC { _mainMiddle :: MiddleS }
  deriving (Eq, Show)

deepLenses ''MainS

stateDeep :: MonadDeepReader r S m => m S
stateDeep = do
  (SC (S1 a)) <- ask
  b <- asks $ \(SC (S1 b)) -> b
  pure (SC (S1 (a + b + 3)))

test_reader :: TestT IO ()
test_reader = do
  a <- runReaderT stateDeep (MainSC (MiddleSC (BotC (SC (S1 5))) (S2 1)))
  SC (S1 13) === a
