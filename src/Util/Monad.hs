module Util.Monad (localState, localState_, anyM) where

import Control.Monad.State (MonadState, get, put)
import Control.Monad.Error (MonadError, catchError)

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

-- | Succeeds if any of the given computations succeeds.
anyM :: MonadError e m
     => [m a] -- ^ computations to try
     -> m a -- ^ computation to return in case of error
     -> m a
anyM (m:ms) em = m `catchError` \_ -> anyM ms em --TODO OPT refactor using Alternative <|>
anyM []     em = em

