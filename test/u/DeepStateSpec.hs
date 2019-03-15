{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE TemplateHaskell #-}

module DeepStateSpec(
  htf_thisModulesTests,
) where

import Control.Monad.Trans.State (execStateT)
import Test.Framework

import Control.Monad.DeepState (MonadDeepState(get, put), gets)
import Data.DeepLenses (deepLenses)

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

stateDeep :: MonadDeepState s S m => m ()
stateDeep = do
  (SC (S1 a)) <- get
  b <- gets $ \(SC (S1 b)) -> b
  put (SC (S1 (a + b + 3)))

test_lens :: IO ()
test_lens = do
  a <- execStateT stateDeep (MainSC (MiddleSC (BotC (SC (S1 5))) (S2 1)))
  assertEqual (MainSC (MiddleSC (BotC (SC (S1 13))) (S2 1))) a
