import System.Environment (getArgs)
import System.Exit        (exitFailure)

import Kx.Par            (pExp, myLexer)
import Kx.Abs

import TypeChecker
import Evaluator

-- | Parse, type check, and interpret a program given by the @String@.

main :: IO ()
-- main = readSrc >>= parse >>= runPipeline
main = runPipeline prg

prg :: Exp
prg = ETLam (Ident "a") (EFLam (Ident "a") (TVar $ Ident "a") (EVar (Ident "a")))

parse :: String -> IO Exp
parse s = do
  case pExp (myLexer s) of
    Left err  -> do
      putStrLn "SYNTAX ERROR"
      putStrLn err
      exitFailure
    Right tree -> return tree

runPipeline :: Exp -> IO ()
runPipeline e = do
  print e
  case typecheck e of
    Left err -> putStrLn err
    Right typed -> do
      let val = evaluate typed
      print val


-- | Main: read file passed by only command line argument and call 'check'.
readSrc :: IO String
readSrc = do
  getArgs >>= \case
    [file] -> readFile file
    _      -> do
      putStrLn "Usage: line <SourceFile>"
      exitFailure
