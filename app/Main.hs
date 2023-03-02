import Types
import Parser
import Evaluator
import Error
import EnvManage
import System.Environment
import System.IO
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad.Except

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
    Left err -> throwError $ Parser err
    Right val -> return val

evalStr :: Env -> String -> IO String
evalStr env expr = runIOThrows $ liftM show $ (liftThrows $ readExpr expr) >>= eval env

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = evalStr env expr >>= putStrLn

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ predicate prompt action = do
  val <- prompt
  if predicate val
  then return ()
  else action val >> until_ predicate prompt action

runOne :: String -> IO ()
runOne expr = nullEnv >>= flip evalAndPrint expr

runRepl :: IO ()
runRepl = nullEnv >>= until_ (== "quit") (readPrompt "Lisp>>> ") . evalAndPrint


main :: IO ()
main = do
  args <- getArgs
  case length args of
    0 -> runRepl
    1 -> runOne $ args !! 0
    _ -> putStrLn "Program requires 0 args for interactive or 1 arg for eval and print"
