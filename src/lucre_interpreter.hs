import Lucretia.Language.Syntax
import Lucretia.Interpreter
-- import qualified Lucretia.Parser.ParsecParser as Parser
import qualified Lucretia.Parser.ApplicativeParser as Parser

import System.IO ( stdin, hGetContents )
import System.Environment ( getArgs, getProgName )

main = do args <- getArgs
          case args of
            [] -> hGetContents stdin >>= runText "<stdin>" 
            fs -> mapM_ runFile  fs

runFile :: FilePath -> IO ()
runFile f = readFile f >>= runText f

runText name text = case Parser.runParser name text of
  Left e -> putStr "Parse error: " >> print e
  Right p ->  do
    putStrLn $ "Parsed OK: " ++ show p
    runProg p
    return ()
