-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Michał Oniszczuk 2011 - 2014
-- Maintainer  :  michal.oniszczuk@gmail.com
--
-- Run shell diff.
-----------------------------------------------------------------------------

module Util.Diff        ( diff ) where
import System.Process   ( readProcessWithExitCode )
import GHC.IO.Exception ( ExitCode )

-- | Run shell diff.
diff :: String -> String -> IO String
diff text1 text2 = do
  (_, output, _) <- readBashCommandWithExitCode $ "diff <(echo '"++text1++"') <(echo '"++text2++"')"
  return output

readBashCommandWithExitCode :: String -> IO (ExitCode, String, String)
readBashCommandWithExitCode command =
  readProcessWithExitCode "bash" ["-c", command] []
