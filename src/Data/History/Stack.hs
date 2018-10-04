{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.History.Stack where

import Control.Monad.Except
import Control.Monad.Free

-- | A stack of effects
data HistoryStack f = PushHistory (f (HistoryStack f))
                    | EmptyHistory

instance Functor f => Semigroup (HistoryStack f) where
  xs <> EmptyHistory = xs
  EmptyHistory <> ys = ys
  ~(PushHistory xs) <> ys = PushHistory $ (<> ys) <$> xs

instance Functor f => Monoid (HistoryStack f) where
  mempty = EmptyHistory

deriving instance Eq (f (HistoryStack f)) => Eq (HistoryStack f)
deriving instance Ord (f (HistoryStack f)) => Ord (HistoryStack f)
deriving instance Show (f (HistoryStack f)) => Show (HistoryStack f)

-- | Hoist one `Functor` to another within a `HistoryStack`
hoistHistoryStack :: Functor g => (forall x. f x -> g x) -> HistoryStack f -> HistoryStack g
hoistHistoryStack _ EmptyHistory = EmptyHistory
hoistHistoryStack f ~(PushHistory x) = PushHistory $ hoistHistoryStack f <$> f x

-- | Wrap a layer around a `HistoryStack`
wrapHistoryStack :: f (HistoryStack f) -> HistoryStack f
wrapHistoryStack = PushHistory

-- | Push a layer onto a `HistoryStack` (at the end, i.e. most recent)
pushHistory :: Functor f => f a -> HistoryStack f -> HistoryStack f
pushHistory xs ys =
  PushHistory $
  case ys of
    EmptyHistory -> EmptyHistory <$ xs
    ~(PushHistory ys') -> pushHistory xs <$> ys'

-- | A `HistoryStack` with a single layer of effects
singletonHistoryStack :: Functor f => f a -> HistoryStack f
singletonHistoryStack = PushHistory . (EmptyHistory <$)

-- | Infinite `HistoryStack` formed by repeating a single layer of effects
foreverHistoryStack :: Functor f => f a -> HistoryStack f
foreverHistoryStack xs = PushHistory $ foreverHistoryStack xs <$ xs

-- | Cut off a `HistoryStack` to be at most the input deep
--
-- We should have:
--
-- @
--  `historyStackDepth` (`cutoffHistoryStack` n xs) <= n
-- @
--
cutoffHistoryStack :: Functor f => Int -> HistoryStack f -> HistoryStack f
cutoffHistoryStack n _ | n <= 0 = mempty
cutoffHistoryStack _ EmptyHistory = EmptyHistory
cutoffHistoryStack n ~(PushHistory xs) = PushHistory $ cutoffHistoryStack (n - 1) <$> xs

-- | Flatten a `HistoryStack` into a single layer
flattenHistoryStack :: Monad m => HistoryStack m -> m ()
flattenHistoryStack EmptyHistory = return ()
flattenHistoryStack ~(PushHistory xs) = xs >>= flattenHistoryStack

-- | Get the depth of a `HistoryStack` while evaluating it
-- a la `flattenHistoryStack`
historyStackDepth :: Monad m => HistoryStack m -> m Int
historyStackDepth EmptyHistory = return 0
historyStackDepth ~(PushHistory xs) = xs >>= fmap (+ 1) . historyStackDepth

-- | At each layer, attempt to `catchError` and return `Pure` if one
-- is caught. Otherwise, repeat all of the layers of the `HistoryStack`
-- indefinitely.
--
-- Note: Since we've already caught any possible errors at each layer,
-- no layer of @m@ in the resulting @`Free` m e@ is `throwError`.
--
catchFreeHistoryStack :: MonadError e m => HistoryStack m -> Free m e
catchFreeHistoryStack xs = loop xs
  where
    loop EmptyHistory = Free . return $ loop xs
    loop ~(PushHistory ys) = Free $ fmap loop ys `catchError` (return . return)

-- | A `HistoryStack` is isomorphic to the `Free` `Monad` of a
-- `Functor`, specialized to the unit type @`()`@.
--
-- See `freeToHistoryStack` for the other half of the isomorphism.
historyStackToFree :: Functor m => HistoryStack m -> Free m ()
historyStackToFree EmptyHistory = Pure ()
historyStackToFree ~(PushHistory x) = Free $ historyStackToFree <$> x

-- | The `Free` `Monad` of a `Functor`, specialized to the unit type @`()`@,
-- is isomorphic to a `HistoryStack`.
--
-- See `historyStackToFree` for the other half of the isomorphism.
freeToHistoryStack :: Functor m => Free m a -> HistoryStack m
freeToHistoryStack (Pure _) = mempty
freeToHistoryStack (Free x) = PushHistory $ freeToHistoryStack <$> x

