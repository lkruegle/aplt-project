import System.Environment (getArgs)
import System.Exit        (exitFailure)

import Kx.Par            (pExp, myLexer)

import TypeChecker
import Evaluator

-- | Parse, type check, and interpret a program given by the @String@.

check :: String -> IO ()
check s = do
  case pExp (myLexer s) of
    Left err  -> do
      putStrLn "SYNTAX ERROR"
      putStrLn err
      exitFailure
    Right tree -> do
      putStrLn $ show tree
      let tree' = typecheck tree
      putStrLn "Type checking passed"
      let val = eval' tree'
      putStrLn $ show val
      return ()

-- | Main: read file passed by only command line argument and call 'check'.

main :: IO ()
main = do
  getArgs >>= \case
    [file] -> readFile file >>= check
    _      -> do
      putStrLn "Usage: line <SourceFile>"
      exitFailure
