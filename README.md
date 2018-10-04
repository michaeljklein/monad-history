# monad-history

This library provides a `Monad` transformer that stores the history of effects in the underlying `Monad`.

The history can be fetched, lifted back to the transformer to run, or reset:

```haskell
class (Monad m, Monad n) => MonadHistory m n | m -> n where
  -- | Get the current `HistoryStack`
  getHistory :: m (HistoryStack n)

  -- | Lift the effect `Monad` to the outer `Monad`
  liftHistory :: n a -> m a

  -- | Reset the `HistoryStack` to `mempty`
  resetHistory :: m ()
```


## Example

The original idea that inspired this library is the phrase
"rinse, wash, repeat" and that it could be encoded in a `Monad` like so:

```haskell
do
  rinse
  wash
  repeat
```

But how do we implement `repeat`?

One way is to get the history and run it forever:

```haskell
rinseWashRepeatForever :: IO ()
rinseWashRepeatForever =
  runHistoryT $ do
    liftIO $ putStrLn "rinse"
    liftIO $ putStrLn "wash"
    liftIO $ putStrLn "repeat"
    getHistory >>= forever . runHistoryStack

-- λ> rinseWashRepeatForever
-- rinse
-- wash
-- repeat
-- rinse
-- wash
-- repeat
-- rinse
-- wash
-- repea^Ct
-- Interrupted.
```


But we can also add conditional exit, e.g. with `ContT`:

```haskell
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
```

