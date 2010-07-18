module Lambdasim.STM (
    stmNew,
    stmRead,
    stmApply,
    stmUpdate,
) where

import Control.Concurrent.STM


stmNew :: a -> IO (TVar a)
stmNew x = atomically (newTVar x)

stmRead :: TVar a -> IO a
stmRead tx = atomically (readTVar tx)

stmApply :: (a -> a) -> TVar a -> IO ()
stmApply f tx = atomically $ do
  x <- readTVar tx
  writeTVar tx $! f x

stmUpdate :: (a -> a) -> TVar a -> IO a
stmUpdate f tx = atomically $ do
  x <- readTVar tx
  let x' = f x
  writeTVar tx $! x'
  return x'
