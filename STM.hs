module STM where

import Control.Concurrent.STM

stmNew :: a -> IO (TVar a)
stmNew x = atomically (newTVar x)

stmRead :: TVar a -> IO a
stmRead x = atomically (readTVar x)

stmApply :: (a -> a) -> TVar a -> IO ()
stmApply f x = atomically (readTVar x >>= writeTVar x . f)

stmUpdate :: (a -> a) -> TVar a -> IO a
stmUpdate f tx = atomically $ do
  x <- readTVar tx
  let x' = f x
  writeTVar tx (f x)
  return x'
