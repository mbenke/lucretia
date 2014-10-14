module Util.Debug ( traceShowIdHl, traceShowIdHlWith ) where

import Debug.Trace ( trace )

import Control.Monad ( mzero, forM )

traceShowIdHl :: (Show a) => a -> a
traceShowIdHl a = trace (
  "\n----------------------------------------\n"
  ++ show a ++
  "\n----------------------------------------\n"
  ) a

traceShowIdHlWith :: (a -> String) -> a -> a
traceShowIdHlWith showA a = trace (
  "\n----------------------------------------\n"
  ++ showA a ++
  "\n----------------------------------------\n"
  ) a

