{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RecordWildCards #-}

module Control.Monad.Trans.Repeat where

import Control.Applicative
import Control.Monad
import Control.Monad.Indexed
import Control.Monad.State
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.Free
import Control.Monad.Trans.HisT
import Control.Monad.Trans.Maybe
import Data.Bifunctor
import Prelude hiding (fail)
import Text.ParserCombinators.ReadPrec ()
import Text.Read ()

-- | Base functor for `RepeatT`
data RepeatF r a where
  Repeat :: (r -> a) -> RepeatF r a
  Break :: r -> RepeatF r a
  Return :: a -> RepeatF r a
  deriving (Functor)

-- | Free monad transformer on `RepeaTF`
newtype RepeatT r m a = RepeatT { runRepeatT :: FreeT (RepeatF r) m a } deriving (Functor)

-- | `Nothing` iff it's `Repeat`, `Left` iff it's `Break`
execRepeatT :: Monad m => RepeatT r m a -> ExceptT r (MaybeT m) a
execRepeatT (RepeatT (FreeT x)) = do
  x' <- (lift . lift) x
  case x' of
    Pure y -> return y
    Free y -> case y of
                Repeat _ -> lift empty
                Break b -> throwE b
                Return z -> execRepeatT $ RepeatT z

instance Monad m => Applicative (RepeatT r m) where
  pure = RepeatT . FreeT . return . Pure

  (<*>) = ap

instance Monad m => Monad (RepeatT r m) where
  return = RepeatT . FreeT . return . Pure

  RepeatT (FreeT x) >>= f = RepeatT . FreeT $ do
    x' <- x
    case x' of
      Pure y -> (runFreeT . runRepeatT . f) y
      Free y -> case y of
                  Repeat r -> return . Free . Repeat . fmap runRepeatT $ (>>= f) . RepeatT . r
                  Break b -> return . Free . Break $ b
                  Return z -> runFreeT . runRepeatT $ RepeatT z >>= f


instance MonadTrans (RepeatT r) where
  lift = RepeatT . lift

instance MonadIO m => MonadIO (RepeatT r m) where
  liftIO = lift . liftIO

instance MonadState s m => MonadState s (RepeatT r m) where
  get = lift get
  put = lift . put
  state = lift . state

-- | Throw a break
breakM :: Monad m => r -> RepeatT r m a
breakM = RepeatT . FreeT . return . Free . Break

-- | Repeat until the previous computations throw a break
repeatM :: Monad m => RepeatT r m r
repeatM = RepeatT . FreeT . return . Free $ Repeat return

-- | Test
test :: Int -> IO ()
test n =
  (>>= print) .
  runMaybeT .
  runExceptT . mapExceptT (mapMaybeT (`evalStateT` (False, 0))) . execRepeatT $ do
    (done, i) <- get
    if done
      then do
        liftIO $ putStrLn "breaking"
        breakM i
      else do
        if i >= n
          then do
            liftIO $ putStrLn "negating initial state"
            modify $ first not
          else do
            liftIO . putStrLn $ "incrementing: " ++ show i
            modify $ fmap (+ 1)
        repeatM

-- | Another attempt to implement `RepeatT`
data RepeaTF r a where
  Brk :: r -> RepeaTF r a
  Rep :: (r -> a) -> RepeaTF r a

instance Functor (RepeaTF r) where
  fmap _ (Brk r) = Brk r
  fmap f (Rep g) = Rep $ f . g

newtype RepeaT r m a = RepeaT { unRepeaT :: FreeT (RepeaTF r) m a } deriving (Functor)

-- | Evaluate `RepeaT`
eval :: Monad m => RepeaT r m a -> ExceptT (RepeaTF r a) m a
eval = mapExceptT evalHisT . evalFT . hoistFreeT lift . unRepeaT

-- | Evaluate free `RepeaTF` with `HisT`
evalFT :: Monad m => FreeT (RepeaTF r) (HisT m) a -> ExceptT (RepeaTF r a) (HisT m) a
evalFT = (>>= evalF) . lift . runFreeT

-- | `undefined`
evalF :: Monad m => FreeF (RepeaTF r) a (FreeT (RepeaTF r) (HisT m) a) -> ExceptT (RepeaTF r a) (HisT m) a
evalF (Pure xs) = return xs
evalF (Free xs) = undefined xs

-- | `undefined`
evalTF :: Monad m => RepeaTF r (FreeT (RepeaTF r) (HisT m) a) -> HisT (ExceptT (RepeaTF r a) m) a
evalTF (Brk r) = undefined r
evalTF (Rep f) = undefined f

-- | Repeat the `Monad`ic action until an exception is thrown
fixLeft :: Monad m => ExceptT a m () -> m a
fixLeft = fix $ liftM2 (>>=) runExceptT . (either return . const <$>)



-- | Indexed monad version of `RepeatT`
--
-- (unfinished)
data RepeaTIx r m i j a where
  RetIx :: a -> RepeaTIx r m i i a
  BrkIx :: r -> RepeaTIx r m i r a
  LiftIx :: m (RepeaTIx r m i j a) -> RepeaTIx r m i j a
  RepeaTIx :: (r -> a) -> RepeaTIx r m r () a

runRepeaTIx :: Monad m => RepeaTIx r m () () a -> MaybeT m a
runRepeaTIx (RetIx x) = return x
runRepeaTIx (BrkIx _) = empty -- => r ~ ()
runRepeaTIx (LiftIx xs) = lift xs >>= runRepeaTIx
runRepeaTIx (RepeaTIx f) = return $ f ()

instance Functor m => IxFunctor (RepeaTIx r m) where
  imap f (RetIx x) = RetIx $ f x
  imap _ (BrkIx r) = BrkIx r
  imap f (LiftIx xs) = LiftIx $ imap f <$> xs
  imap f (RepeaTIx g) = RepeaTIx $ f . g

instance Functor m => IxPointed (RepeaTIx r m) where
  ireturn = RetIx

-- | `undefined`
instance Applicative m => IxApplicative (RepeaTIx r m) where
  iap (RetIx f) xs = imap f xs
  iap fs (RetIx x) = imap ($ x) fs
  iap _ (BrkIx r)  = BrkIx r
  iap (BrkIx r) (LiftIx xs)  = LiftIx $ iap (BrkIx r) <$> xs
  iap (BrkIx r) (RepeaTIx xs)  = (undefined :: r -> (r -> a) -> RepeaTIx r m i () b) r xs
  iap (LiftIx fs) (LiftIx xs) = LiftIx $ liftA2 iap fs xs

