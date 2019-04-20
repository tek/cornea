{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE TemplateHaskell #-}

module DeepErrorSpec(
  htf_thisModulesTests,
) where

import Control.Monad (when)
import Control.Monad.Trans.Except (runExceptT)
import Data.Foldable (traverse_)
import Language.Haskell.TH
import Test.Framework

import Control.Monad.DeepError (MonadDeepError(throwHoist))
import Data.DeepPrisms (deepPrisms)

newtype Err1 =
  Err1 Int
  deriving (Eq, Show)

newtype Err2 =
  Err2C Int
  deriving (Eq, Show)

data Err =
  ErrC Err1
  |
  ErrC1 Err2
  deriving (Eq, Show)

deepPrisms ''Err

data Bot =
  BotC Err
  |
  BotOther Err2
  deriving (Eq, Show)

deepPrisms ''Bot

newtype MiddleOther =
  MiddleOther Int
  deriving (Eq, Show)

data MiddleErr =
  MiddleErrC Bot
  |
  MiddleErrOther MiddleOther
  deriving (Eq, Show)

deepPrisms ''MiddleErr

newtype MainOther =
  MainOther Int
  deriving (Eq, Show)

data MainErr =
  MainErrC MiddleErr
  |
  MainErrOther MainOther
  deriving (Eq, Show)

deepPrisms ''MainErr

throwDeep :: MonadDeepError e Err m => m ()
throwDeep =
  throwHoist (ErrC (Err1 5))

debugPrint :: IO ()
debugPrint =
  traverse_ putStrLn $ lines $(stringE . pprint =<< deepPrisms ''Err2)

test_hoist :: IO ()
test_hoist = do
  when debug debugPrint
  a <- runExceptT throwDeep
  assertEqual (Left (MainErrC (MiddleErrC (BotC (ErrC (Err1 5)))))) a
  where
    debug = False
