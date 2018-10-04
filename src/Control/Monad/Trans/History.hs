{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.Trans.History where

import Control.Monad.Except
import Control.Monad.Free
import Control.Monad.State
import Control.Monad.Trans.Accum
import Control.Monad.Trans.Accum
import Control.Monad.Trans.Class
import Control.Monad.Trans.Cont
import Control.Monad.Trans.Free
import Control.Monad.Trans.Identity
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.RWS hiding (get, put, state)
import qualified Control.Monad.Trans.RWS.Strict as S
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Select
import Control.Monad.Trans.State hiding (get, put, state)
import qualified Control.Monad.Trans.State.Strict as S
import Control.Monad.Trans.Writer
import qualified Control.Monad.Trans.Writer.Strict as S
import Data.History.Stack


-- | Adds a history of all effects that can be fetched, run, and reset.
--
-- Note: Only effects in @m@ are stored in the history so updates to the
-- `HistoryStack` `StateT` variable will be ignored during history replays.
--
-- See `historyExample` and `historyExample2` for examples.
--
-- See `MonadHistory` for a `MonadTrans`former type-class a la @mtl@
newtype HistoryT m a = HistoryT
  { unHistoryT :: StateT (HistoryStack m) m a
  } deriving (Functor)

-- | Run a `HistoryT` computation
runHistoryT :: Monad m => HistoryT m a -> m a
runHistoryT = (`evalStateT` mempty) . unHistoryT

-- | Because `HistoryT` includes the `Functor` variable
-- in both the argument and result of the `StateT` `Monad`,
-- we need natural transformations to and from @n@ to hoist
-- @m -> n@
hoistHistoryT ::
     (Functor n, Functor m)
  => (forall x. m x -> n x)
  -> (forall y. n y -> m y)
  -> HistoryT m a
  -> HistoryT n a
hoistHistoryT f g =
  HistoryT .
  StateT .
  fmap (fmap (fmap (hoistHistoryStack f)) . f) .
  (. hoistHistoryStack g) . runStateT . unHistoryT


instance Monad m => Applicative (HistoryT m) where
  pure = HistoryT . return

  (<*>) = ap

instance Monad m => Monad (HistoryT m) where
  HistoryT (StateT xs) >>= f =
    HistoryT . StateT $ \h -> do
      let xs' = xs h
      ~(x, xh) <- xs'
      let ~(StateT ys) = unHistoryT $ f x
          ys' = ys $ pushHistory xs' xh
      ~(y, yh) <- ys'
      return (y, pushHistory ys' yh)

instance Monad m => MonadState (HistoryStack m) (HistoryT m) where
  get = HistoryT get
  put = HistoryT . put
  state = HistoryT . state

instance MonadTrans HistoryT where
  lift = HistoryT . lift

instance MonadIO m => MonadIO (HistoryT m) where
  liftIO = HistoryT . liftIO

-- | `Monad`s @m@ that support the features provided by `HistoryT`;
-- namely: getting, lifting and resetting a `HistoryStack` of effects in @n@.
--
-- `MonadHistory` instances should satisfy:
--
-- @
--  `resetHistory` `>>` `getHistory` = `return` `mempty`
--  `resetHistory` `>>` `liftHistory` f `>>` `getHistory` = `resetHistory` `>>` `liftHistory` f `>>` `return` (`singletonHistoryStack` f)
--  `getHistory` `>>=` \\h0 -> `liftHistory` f `>>` `getHistory` `>>=` \\h1 -> (h0, h1) = `getHistory` `>>=` \\h0 -> `liftHistory` f `>>` `return` (h0, `pushHistory` f h0)
-- @
--
-- Notes:
--
-- - The second rule can be derived from the first and last.
-- - Only effects in @n@ are stored in the history
--
class (Monad m, Monad n) => MonadHistory m n | m -> n where
  -- | Get the current `HistoryStack`
  getHistory :: m (HistoryStack n)

  -- | Lift the effect `Monad` to the outer `Monad`
  liftHistory :: n a -> m a

  -- | Reset the `HistoryStack` to `mempty`
  resetHistory :: m ()

instance Monad m => MonadHistory (HistoryT m) m where
  getHistory = HistoryT get
  liftHistory = lift
  resetHistory = HistoryT $ put mempty

instance MonadHistory m n => MonadHistory (ExceptT e m) n where
  getHistory = lift getHistory
  liftHistory = lift . liftHistory
  resetHistory = lift resetHistory

instance MonadHistory m n => MonadHistory (IdentityT m) n where
  getHistory = lift getHistory
  liftHistory = lift . liftHistory
  resetHistory = lift resetHistory

instance MonadHistory m n => MonadHistory (SelectT s m) n where
  getHistory = lift getHistory
  liftHistory = lift . liftHistory
  resetHistory = lift resetHistory

instance MonadHistory m n => MonadHistory (StateT s m) n where
  getHistory = lift getHistory
  liftHistory = lift . liftHistory
  resetHistory = lift resetHistory

instance MonadHistory m n => MonadHistory (S.StateT s m) n where
  getHistory = lift getHistory
  liftHistory = lift . liftHistory
  resetHistory = lift resetHistory

instance (MonadHistory m n, Monoid w) => MonadHistory (WriterT w m) n where
  getHistory = lift getHistory
  liftHistory = lift . liftHistory
  resetHistory = lift resetHistory

instance (MonadHistory m n, Monoid w) => MonadHistory (S.WriterT w m) n where
  getHistory = lift getHistory
  liftHistory = lift . liftHistory
  resetHistory = lift resetHistory

instance (MonadHistory m n, Monoid w) => MonadHistory (AccumT w m) n where
  getHistory = lift getHistory
  liftHistory = lift . liftHistory
  resetHistory = lift resetHistory

instance MonadHistory m n => MonadHistory (ContT r m) n where
  getHistory = lift getHistory
  liftHistory = lift . liftHistory
  resetHistory = lift resetHistory

instance MonadHistory m n => MonadHistory (ReaderT r m) n where
  getHistory = lift getHistory
  liftHistory = lift . liftHistory
  resetHistory = lift resetHistory

instance (MonadHistory m n, Monoid w) => MonadHistory (RWST r w s m) n where
  getHistory = lift getHistory
  liftHistory = lift . liftHistory
  resetHistory = lift resetHistory

instance (MonadHistory m n, Monoid w) => MonadHistory (S.RWST r w s m) n where
  getHistory = lift getHistory
  liftHistory = lift . liftHistory
  resetHistory = lift resetHistory

instance MonadHistory m n => MonadHistory (MaybeT m) n where
  getHistory = lift getHistory
  liftHistory = lift . liftHistory
  resetHistory = lift resetHistory

instance (MonadHistory m n, Functor f) => MonadHistory (FreeT f m) n where
  getHistory = lift getHistory
  liftHistory = lift . liftHistory
  resetHistory = lift resetHistory


-- | Run a `HistoryStack`
runHistoryStack :: MonadHistory m n => HistoryStack n -> m ()
runHistoryStack = liftHistory . flattenHistoryStack

-- | Reset the `HistoryStack` before throwing the error
breakHistory :: (MonadHistory m n, MonadError e n) => e -> m a
breakHistory err = do
  resetHistory
  liftHistory $ throwError err


-- | Example with `getHistory`, `runHistoryStack` and `resetHistory`:
--
-- @
--  historyExample :: IO ()
--  historyExample = runHistoryT $ do
--    let p = liftIO . (print :: Int -> IO ())
--        ps = liftIO . putStrLn
--    p 1
--    p 2
--    p 3
--    history1 <- getHistory
--    ps "repeating history up to now"
--    runHistoryStack history1
--    ps "(nothing else is repeated)"
--
--    ps ""
--    ps "Now we reset:"
--    resetHistory
--
--    p 42
--    ps "^ it's a number"
--
--    history2 <- getHistory
--    ps "repeating history up to now"
--    runHistoryStack history2
--    ps "(nothing else is repeated)"
-- @
--
-- Results:
--
-- @
--  λ> historyExample
--  1
--  2
--  3
--  repeating history up to now
--  1
--  2
--  3
--  (nothing else is repeated)
--
--  Now we reset:
--  42
--  ^ it's a number
--  repeating history up to now
--  42
--  ^ it's a number
--  (nothing else is repeated)
-- @
--
historyExample :: IO ()
historyExample = runHistoryT $ do
  let p = liftIO . (print :: Int -> IO ())
      ps = liftIO . putStrLn
  p 1
  p 2
  p 3
  history1 <- getHistory
  ps "repeating history up to now"
  runHistoryStack history1
  ps "(nothing else is repeated)"
  --
  ps ""
  ps "Now we reset:"
  resetHistory
  --
  p 42
  ps "^ it's a number"
  --
  history2 <- getHistory
  ps "repeating history up to now"
  runHistoryStack history2
  ps "(nothing else is repeated)"


-- | Simple example:
--
-- @
--  historyExample2 :: IO ()
--  historyExample2 =
--    runHistoryT $ do
--      liftIO $ putStrLn "repeat me 1"
--      liftIO $ putStrLn "repeat me 2"
--      liftIO $ putStrLn "repeat me 3"
--      history1 <- getHistory
--      liftIO $ putStrLn "repeating:"
--      stackDepth <- lift $ historyStackDepth history1
--      liftIO . putStrLn $ unwords ["Repeated", show stackDepth, "layers of IO"]
-- @
--
-- Results:
--
-- @
--  λ> historyExample2
--  repeat me 1
--  repeat me 2
--  repeat me 3
--  repeating:
--  repeat me 1
--  repeat me 2
--  repeat me 3
--  Repeated 3 layers of IO
-- @
--
historyExample2 :: IO ()
historyExample2 =
  runHistoryT $ do
    liftIO $ putStrLn "repeat me 1"
    liftIO $ putStrLn "repeat me 2"
    liftIO $ putStrLn "repeat me 3"
    history <- getHistory
    liftIO $ putStrLn "repeating:"
    stackDepth <- lift $ historyStackDepth history
    liftIO . putStrLn $ unwords ["Repeated", show stackDepth, "layers of IO"]

-- | Rinse, wash and repeat
--
-- @
--  λ> rinseWashRepeat
--  rinse
--  wash
--  repeat?
--  True
--  rinse
--  wash
--  repeat?
--  False
--  all done!
-- @
--
rinseWashRepeat :: IO ()
rinseWashRepeat =
  runHistoryT $ do
    liftIO $ putStrLn "rinse"
    liftIO $ putStrLn "wash"
    liftIO $ putStrLn "repeat?"

    history <- getHistory
    (evalContT . callCC) $ \exit -> do
      forever $ do
        repeatIt <- read <$> liftIO getLine
        if repeatIt
           then runHistoryStack history
           else exit ()
    liftIO $ putStrLn "all done!"

-- | `rinseWashRepeat` with no end condition:
--
-- @
--  λ> rinseWashRepeatForever
--  rinse
--  wash
--  repeat
--  rinse
--  wash
--  repeat
--  rinse
--  wash
--  repea^Ct
--  Interrupted.
-- @
--
rinseWashRepeatForever :: IO ()
rinseWashRepeatForever =
  runHistoryT $ do
    liftIO $ putStrLn "rinse"
    liftIO $ putStrLn "wash"
    liftIO $ putStrLn "repeat"
    getHistory >>= forever . runHistoryStack

