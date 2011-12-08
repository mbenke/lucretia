module Debug(debugPrint, debugPrintList) where

import System.IO.Unsafe(unsafePerformIO)
import Control.Monad(mzero, forM)

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
