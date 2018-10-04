{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}

module Control.Monad.Trans.Counter where

import Data.Bifunctor
import Control.Monad.State
import Control.Monad.Trans.Class

-- | Add a counter to `Monad`ic operations, where a count of @1@ is added
-- for `fmap`, @(`<*>`)@, and @(`>>=`)@
newtype CounterT m a = CounterT { unCounterT :: Int -> m (a, Int) }

-- | Initial count is @0@
runCounterT :: CounterT m a -> m (a, Int)
runCounterT = ($ 0) . unCounterT

-- | Add @1@ when `fmap`ping
instance Functor m => Functor (CounterT m) where
  fmap f = CounterT . fmap (fmap (bimap f (+ 1))) . unCounterT

-- | Add @1@ when applying
instance Monad m => Applicative (CounterT m) where
  pure x = CounterT $ \i -> return (x, i)

  CounterT fs <*> CounterT xs = CounterT $ \i -> do
    ~(f, j) <- fs $ i + 1
    first f <$> xs j

-- | Add @1@ when binding
instance Monad m => Monad (CounterT m) where
  CounterT x >>= f = CounterT $ \i -> x i >>= \(~(y, j)) -> f y `unCounterT` (j + 1)

instance MonadTrans CounterT where
  lift xs = CounterT $ \x -> (, x) <$> xs

instance Monad m => MonadState Int (CounterT m) where
  get = CounterT $ return . join (,)
  put = CounterT . return . return . ((), )
  state = CounterT . fmap return

