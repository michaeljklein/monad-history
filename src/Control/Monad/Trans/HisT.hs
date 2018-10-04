{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Control.Monad.Trans.HisT where

import Data.History.Stack
import Control.Monad.State.Strict
import Control.Monad.Trans.Class
import Control.Monad.Trans.History


-- | Monad transformer that includes the history of effects
-- that lead up to the current value
newtype HisT m a = HisT { unHisT :: StateT (m ()) m a } deriving (Functor)

-- | Run a `HisT` computation
evalHisT :: Monad m => HisT m a -> m a
evalHisT = (`evalStateT` return ()) . unHisT

instance Monad m => Applicative (HisT m) where
  pure = HisT . pure

  (<*>) = ap

instance Monad m => Monad (HisT m) where
  HisT (StateT xs) >>= f = HisT . StateT $ \h -> do
    (y, hy) <- xs h
    (z, hz) <- (runStateT . unHisT . f) y hy
    return (z, hy >> hz)

instance MonadTrans HisT where
  lift = HisT . lift

instance Monad m => MonadState (m ()) (HisT m) where
  get :: HisT m (m ())
  get = HisT get

  put :: m () -> HisT m ()
  put = HisT . put

  state :: (m () -> (a, m ())) -> HisT m a
  state = HisT . state

-- | `HisT` only supports a single-layer `HistoryStack`
instance Monad m => MonadHistory (HisT m) m where
  getHistory = singletonHistoryStack <$> get
  liftHistory = lift
  resetHistory = put $ return ()

