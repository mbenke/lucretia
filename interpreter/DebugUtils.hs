module DebugUtils where

import Debug.Trace

import System.IO.Unsafe(unsafePerformIO)
import Control.Monad(mzero, forM)

traceShowId :: (Show a) => a -> a
traceShowId a = traceShow a a

traceShowIdHl :: (Show a) => a -> a
traceShowIdHl a = trace (
  "\n----------------------------------------\n"
  ++ show a ++
  "\n----------------------------------------\n"
  ) a


debugPrint msg = 
  unsafePerformIO $ do
    putStrLn $ "----------------------------------------"
    putStrLn $ msg
    putStrLn $ "----------------------------------------"
    return mzero

debugPrintList msgs = 
  unsafePerformIO $ do
    putStrLn $ "----------------------------------------"
    forM msgs putStrLn
    putStrLn $ "----------------------------------------"
    return mzero
