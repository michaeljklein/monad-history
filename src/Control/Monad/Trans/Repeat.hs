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

import Data.Bifunctor
-- import Data.Type.Equality
import Control.Monad.Trans.Except
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
-- import Data.Functor.Classes
-- import qualified Text.ParserCombinators.ReadP as RP
import Control.Applicative
import Text.ParserCombinators.ReadPrec ()
import Text.Read ()
import Prelude hiding (fail)
-- import Control.Monad.Fail
import Control.Monad.Trans.Free
import Control.Monad
import Control.Monad.State
-- import Control.Monad.IO.Class

import Control.Monad.Indexed
import Control.Monad.Trans.HisT


-- data RepeatT r m a where
--   Repeat :: (r -> a) -> RepeatT r m a
--   ToRepeat :: ExceptT r m a -> RepeatT r m a

-- repeat :: RepeatT r m r
-- repeat = Repeat id

-- instance Functor m => Functor (RepeatT r m) where
--   fmap f (Repeat g) = Repeat (f . g)
--   fmap f (ToRepeat xs) = ToRepeat $ f <$> xs

-- instance Monad m => Applicative (RepeatT r m) where
--   pure = ToRepeat . pure

--   Repeat f <*> Repeat x = Repeat $ f <*> x
--   Repeat f <*> ToRepeat (ExceptT xs) = _ f xs
--   ToRepeat (ExceptT fs) <*> Repeat x = ToRepeat . ExceptT $ fs >>= either (_ x) (_ . (. x))
--   ToRepeat fs <*> ToRepeat xs = ToRepeat $ fs <*> xs



-- Ok, so it looks like the case on line 40 should return (Repeat) if `Right` and (ToRepeat (ExceptT (Left l))) if `Left`

-- To make that work, `RepeatT` will need:
--   wrap :: Monad m => m (RepeatT r m a) -> RepeatT r m a


data RepeatF r a where
  Repeat :: (r -> a) -> RepeatF r a
  Break :: r -> RepeatF r a
  Return :: a -> RepeatF r a
  deriving (Functor)

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

breakM :: Monad m => r -> RepeatT r m a
breakM = RepeatT . FreeT . return . Free . Break

repeatM :: Monad m => RepeatT r m r
repeatM = RepeatT . FreeT . return . Free $ Repeat return

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



-- data RepeaT r m a where
--   Brk :: r -> RepeaT r m a
--   RepeaT :: (r -> _ a) -> RepeaT r m a


-- Stack of effects that could break, Repeat, Another stack of effects that could break, Repeat ..

-- ExceptT r m (a | (r -> a) -> _ |

-- m (r | a | (r -> RepeatT r m a) -> _

data RepeaTF r a where
  Brk :: r -> RepeaTF r a
  Rep :: (r -> a) -> RepeaTF r a

instance Functor (RepeaTF r) where
  fmap _ (Brk r) = Brk r
  fmap f (Rep g) = Rep $ f . g

newtype RepeaT r m a = RepeaT { unRepeaT :: FreeT (RepeaTF r) m a } deriving (Functor)

eval :: Monad m => RepeaT r m a -> ExceptT (RepeaTF r a) m a
eval = mapExceptT evalHisT . evalFT . hoistFreeT lift . unRepeaT

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

-- xs = runExceptT xs >>= either return (const $ fixLeft xs)

-- \f -> liftM2 (>>=) runExceptT (either return . const . f)

-- ExceptT (RepeaTF r a) m ()


-- Bind and carry stack

-- If Brk:
--   Drop stack
-- If Rep:
--   Fix stack to get result

-- FreeT (RepeaTF r) m a -> FreeT (RepeaTF r) (HisT m) a



-- m a -> (a -> m b) -> m b
-- (m (), m a) -> (a -> m b) -> (m (), m b)


    -- xs >>= _ . f

  {-
newtype HisT m a = HisT
  { runHisT :: (m (), m a)
  }

evalHisT :: HisT m a -> m a
evalHisT = snd . runHisT

instance Functor m => Functor (HisT m) where
  fmap f = HisT . fmap (fmap f) . runHisT

instance Applicative m => Applicative (HisT m) where
  pure x = HisT (pure (), pure x)
  ~(HisT (fh, fs)) <*> ~(HisT (xh, xs)) = HisT (fh *> xh, fs <*> xs)

instance Monad m => Monad (HisT m) where
  ~(HisT (xh, xs)) >>= f =
    HisT (xs >>= fst . runHisT . f, xs >>= snd . runHisT . f)

instance MonadTrans HisT where
  lift xs = HisT (void xs, xs)
-}


-- -- | Indexed monad version of `RepeatT`
-- data RepeaT r m i j a where
--   Ret :: a -> RepeaT r m i i a
--   Brk :: r -> RepeaT r m i r a
--   Lift :: m (RepeaT r m i j a) -> RepeaT r m i j a
--   RepeaT :: (r -> a) -> RepeaT r m r () a

-- runRepeaT :: Monad m => RepeaT r m () () a -> MaybeT m a
-- runRepeaT (Ret x) = return x
-- runRepeaT (Brk _) = empty -- => r ~ ()
-- runRepeaT (Lift xs) = lift xs >>= runRepeaT
-- runRepeaT (RepeaT f) = return $ f ()

-- instance Functor m => IxFunctor (RepeaT r m) where
--   imap f (Ret x) = Ret $ f x
--   imap _ (Brk r) = Brk r
--   imap f (Lift xs) = Lift $ imap f <$> xs
--   imap f (RepeaT g) = RepeaT $ f . g

-- instance Functor m => IxPointed (RepeaT r m) where
--   ireturn = Ret

-- instance Applicative m => IxApplicative (RepeaT r m) where
--   iap (Ret f) xs = imap f xs
--   iap fs (Ret x) = imap ($ x) fs
--   iap _ (Brk r)  = Brk r
--   iap (Brk r) (Lift xs)  = Lift $ iap (Brk r) <$> xs
--   iap (Brk r) (RepeaT xs)  = (undefined :: r -> (r -> a) -> RepeaT r m i () b) r xs
--   iap (Lift fs) (Lift xs) = Lift $ liftA2 iap fs xs




-- data RepeaT r m a where
--   Ret :: a -> RepeaT r m a
--   Lift :: m (RepeaT r m a) -> RepeaT r m a
--   RepeaT :: ExceptT r m x -> (r -> RepeaT r m a) -> RepeaT r m a

-- -- wrapRepeaT :: Monad m => m (RepeaT r m a) -> RepeaT r m a
-- -- wrapRepeaT xs = RepeaT (_ xs) (_ xs)

-- liftRepeaT :: Monad m => m a -> RepeaT a m a
-- liftRepeaT xs = RepeaT (ExceptT $ Left <$> xs) Ret

-- instance Monad m => Functor (RepeaT r m) where
--   fmap f (Ret x) = Ret $ f x
--   fmap f (Lift xs) = Lift $ fmap f <$> xs
--   fmap f (RepeaT xs g) = RepeaT xs $ fmap f . g

-- instance Monad m => Applicative (RepeaT r m) where
--   pure = Ret

--   Ret f <*> xs = f <$> xs
--   f <*> Ret x = ($ x) <$> f
--   Lift fs <*> xs = Lift $ (<*> xs) <$> fs
--   fs <*> Lift xs = Lift $ (fs <*>) <$> xs
--   RepeaT fx fs <*> RepeaT xx xs = Lift $ (fromLeftM $ fx >> xx) >>= return . liftA2 (<*>) fs xs


-- fromLeftM :: Monad m => ExceptT a m x -> m a
-- fromLeftM ~(ExceptT xs) = loop
--   where
--     loop = xs >>= either return (const loop)



-- Break short-circuits to RepeaT

-- Break | RepeaT | Middle

-- Middle :: ExceptT r m a
-- RepeaT :: (r -> a) -> RepeaT r m a





  -- Lift fs <*> Lift xs = Lift $ liftA2 (<*>) fs xs
  -- RepeaT fs <*> RepeaT xs = RepeaT $ liftA2 (<*>) fs xs
  -- (Lift (ExceptT fs)) <*> (RepeaT xs) = Lift . lift $ fs >>= either (_ . xs) (return . RepeaT . _ xs)
  -- (RepeaT fs) <*> (Lift xs) = _ fs xs


-- data RepeaT m i j a where
--   Ret :: a -> RepeaT m i i a
--   Break :: i -> RepeaT m i r a
--   RepeaT :: (i -> a) -> RepeaT m i i a



-- (RepeatT r m a) is the free monad with the following additions:
-- - Break throws a result of type r
-- - Repeat repeats all previous after break if exists or returns on break if last is break
-- - Any other is a monadic computation




-- Hmmmmm


-- Break b >>= \_ -> Return = Break b
-- Return >>= \_ -> Break b = Break b
-- Return >>= \_ -> Repeat r = Repeat r
-- Break b >>= \_ -> Repeat r = Return $ r b
-- Repeat x >>= f = Repeat $ (>>= f) . x


{-
-- | Here, `Nothing` stands for repeating
newtype RepeatT r m a = MaybeT (ExceptT r m) a

repeat :: RepeatT r m r
repeat = RepeatT Nothing

-- | Here, `Nothing` stands for a loop failure, i.e. the input was just `repeat`
runRepeatT :: RepeatT r m a -> MaybeT (ExceptT r m) a

- Should we allow the exceptions in the result?
  * Ideally, we'd have something like "exceptions can only be thrown to an instance of `repeat`"
- (xs *> repeat) should be: fromLeft (forever xs)


data RepeatT s r m a where
  Repeat :: RepeatT ThrowsExceptions r m a -> RepeatT NoExceptions r m r
  ThrowException :: r -> RepeatT ThrowsExceptions r m a
  LiftRepeat :: m a -> RepeatT NoExceptions r m a
  LiftRepeat :: m a -> RepeatT _ r m a ??

It's not a Functor:
vvvvvvvvvvvvvvvvvvvvvvvv

data RepeatT r m a where
  Repeat :: RepeatT r m r
  ToRepeat :: ExceptT r m a -> RepeatT r m a

instance (Eq r, Eq1 m) => Eq1 (RepeatT r m) where
  liftEq _ Repeat Repeat = True
  liftEq _ Repeat _ = False
  liftEq _ _ Repeat = False
  liftEq eq (ToRepeat xs) (ToRepeat ys) = liftEq eq xs ys

instance (Ord r, Ord1 m) => Ord1 (RepeatT r m) where
  liftCompare _ Repeat Repeat = EQ
  liftCompare _ Repeat _ = LT
  liftCompare _ _ Repeat = GT
  liftCompare cmp (ToRepeat xs) (ToRepeat ys) = liftCompare cmp xs ys

instance (Show r, Show1 m) => Show1 (RepeatT r m) where
  liftShowsPrec _ _ _ Repeat = showString "Repeat"
  liftShowsPrec sp sl n (ToRepeat xs) = liftShowsPrec sp sl n xs

instance (Eq r, Eq1 m, Eq a) => Eq (RepeatT r m a) where
  (==) = eq1

instance (Ord r, Ord1 m, Ord a) => Ord (RepeatT r m a) where
  compare = compare1

instance (Show r, Show1 m, Show a) => Show (RepeatT r m a) where
  showsPrec = showsPrec1


instance (Read r, Read1 m, Read a, ReadRepeat r a (r == a)) => Read (RepeatT r m a) where
  readPrec = readP_to_Prec (const readRepeat) <|> (ToRepeat <$> readPrec)

class ((r == a) ~ b) => ReadRepeat (r :: *) (a :: *) (b :: Bool) where
  readRepeat :: RP.ReadP (RepeatT r m a)

instance ((r == r) ~ 'True) => ReadRepeat r r 'True where
  readRepeat = RP.string "Repeat" >> return Repeat

instance ((r == a) ~ 'False) => ReadRepeat r a 'False where
  readRepeat = fail "r /= a"


instance Functor m => Functor (RepeatT r m) where
  fmap _ Repeat = _
  fmap f (ToRepeat xs) = ToRepeat $ f <$> xs

repeat :: RepeatT r m r
repeat = Repeat

toRepeat :: ExceptT r m a -> RepeatT r m a
toRepeat = ToRepeat
-}
