import Desugar
import Evaluator
import qualified Kx.Abs as A
import Types
import Kx.Par (myLexer, pExp)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import TypeChecker

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
    Right tree -> return tree

runDesugar :: A.Exp -> IO Exp
runDesugar a = return $ desugar a

runTypechecker :: Exp -> IO (Inferred '[])
runTypechecker e = case typecheck e of
    Right prg -> return prg
    Left err -> do
      putStrLn $ unlines ["Typechecking Failed:", err]
      exitFailure

runEvaluator :: Inferred '[] -> IO ()
runEvaluator (Inferred term) = print . evaluate $ term
