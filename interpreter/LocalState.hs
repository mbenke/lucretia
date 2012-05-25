module LocalState (localState, localState_) where

import Control.Monad.State

localState :: MonadState s m => m a -> m a
localState m = do
  s <- get
  r <- m
  put s
  return r

localState_ :: MonadState s m => m a -> m ()
localState_ m = do
  s <- get
  m
  put s
