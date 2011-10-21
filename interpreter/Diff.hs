module Diff(diff) where
import System.Process(readProcessWithExitCode)

diff :: String -> String -> IO String
diff text1 text2 = do
  (_, output, _) <- readBashCommandWithExitCode $ "diff <(echo '"++text1++"') <(echo '"++text2++"')"
  return output

--readBashCommandWithExitCode :: String -> IO (GHC.IO.Exception.ExitCode, String, String)
readBashCommandWithExitCode command =
  readProcessWithExitCode "bash" ["-c", command] []
