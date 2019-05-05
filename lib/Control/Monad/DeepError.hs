module Control.Monad.DeepError where

import Control.Exception (Exception, IOException, SomeException)
import Control.Monad.Error.Class (MonadError(throwError, catchError))
import Control.Monad.Trans.Control (MonadBaseControl)

import Data.DeepPrisms (DeepPrisms, hoist, retrieve)
import Data.Either.Combinators (mapLeft)

class (MonadError e m, DeepPrisms e e') => MonadDeepError e e' m where
  throwHoist :: e' -> m a

instance (MonadError e m, DeepPrisms e e') => MonadDeepError e e' m where
  throwHoist =
    throwError . hoist

catchAt :: ∀ e' e m a. (MonadError e m, DeepPrisms e e') => (e' -> m a) -> m a -> m a
catchAt handle ma =
  catchError ma f
  where
    f e = maybe (throwError e) handle (retrieve e)

hoistEither :: MonadDeepError e e' m => Either e' a -> m a
hoistEither =
  either throwHoist return

hoistEitherWith :: MonadDeepError e e'' m => (e' -> e'') -> Either e' a -> m a
hoistEitherWith f =
  hoistEither . mapLeft f

hoistMaybe ::
  MonadDeepError e e' m =>
  e' ->
  Maybe a ->
  m a
hoistMaybe e' =
  maybe (throwHoist e') return

tryHoist ::
  MonadBaseControl IO m =>
  MonadDeepError e e' m =>
  Exception ex =>
  (ex -> e') ->
  m ()
tryHoist =
  undefined

tryHoistIO ::
  MonadBaseControl IO m =>
  MonadDeepError e e' m =>
  (IOException -> e') ->
  m ()
tryHoistIO =
  undefined

tryHoistIOAs ::
  MonadBaseControl IO m =>
  MonadDeepError e e' m =>
  e' ->
  m ()
tryHoistIOAs =
  undefined

tryHoistAny ::
  MonadBaseControl IO m =>
  MonadDeepError e e' m =>
  (SomeException -> e') ->
  m ()
tryHoistAny =
  undefined

tryHoistAnyAs ::
  MonadDeepError e e' m =>
  e' ->
  m ()
tryHoistAnyAs =
  undefined

-- TODO derive multiple errors with HList + Generic
