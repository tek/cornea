module DeepErrorSpec where

import Control.Monad.DeepError (MonadDeepError(throwHoist))
import Data.DeepPrisms (deepPrisms)
import qualified Data.String as String (lines)
import Hedgehog (TestT, (===))
import Language.Haskell.TH

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
  traverse_ putStrLn . String.lines $ $(stringE . pprint =<< deepPrisms ''Err2)

test_hoist :: TestT IO ()
test_hoist = do
  liftIO (when debug debugPrint)
  a <- runExceptT throwDeep
  Left (MainErrC (MiddleErrC (BotC (ErrC (Err1 5))))) === a
  where
    debug = False
