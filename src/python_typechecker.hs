main :: IO ()
main = do
  pythonSource <- getContents

  let lucretiaSource = pyToLu pythonSource
  --verbose <- (== "-v") args 1
  let verbose = True
  if verbose then show lucretiaSource
	     else return ()
  case typecheck lucretiaSource of
    Left err -> putStrLn err
    Right _ -> putStrLn "No errors found."
