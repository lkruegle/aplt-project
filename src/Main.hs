import Desugar
import Evaluator
import qualified Kx.Abs as A
import Kx.Par (myLexer, pExp)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import TypeChecker
import Types

-- | Parse, type check, and interpret a program given by the @String@.
main :: IO ()
main =
  readSrc
    >>= runParser
    >>= runDesugar
    >>= runTypechecker
    >>= runEvaluator

readSrc :: IO String
readSrc = do
  getArgs >>= \case
    [file] -> readFile file
    _ -> do
      putStrLn "Usage: line <SourceFile>"
      exitFailure

runParser :: String -> IO A.Exp
runParser s = do
  case pExp (myLexer s) of
    Left err -> do
      putStrLn "SYNTAX ERROR"
      putStrLn err
      exitFailure
    Right tree -> do
      -- print tree
      return tree

runDesugar :: A.Exp -> IO Exp
runDesugar a = do
  let p = desugar a
  -- print p
  return p

runTypechecker :: Exp -> IO Exp
runTypechecker e = case typecheck e of
  Right t -> do
    -- print t
    return e
  Left err -> do
    putStrLn $ unlines ["Typechecking Failed:", err]
    exitFailure

runEvaluator :: Exp -> IO ()
runEvaluator = putStrLn . showVal . evaluate
