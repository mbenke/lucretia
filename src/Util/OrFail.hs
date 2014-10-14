module Util.OrFail (orFail, orFailE) where

import Control.Monad.Error
import Data.Monoid

orFail :: MonadError e m => Bool -> e -> m ()
orFail cond errorMsg =
  unless cond $ throwError errorMsg

orFailE :: (Monoid e, MonadError e m) => Either e a -> e -> m ()
orFailE (Left msg') msg = throwError $ mappend msg msg'
orFailE (Right _)   _   = return ()

