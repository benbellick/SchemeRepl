import Types
import Parser
import Reader
import Evaluator
import EnvManage
import System.Environment
import System.IO
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad.Except

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

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

runOne :: [String] -> IO ()
--runOne expr = primitiveBindings >>= flip evalAndPrint expr
runOne args = do
  env <- primitiveBindings >>= flip bindVars [("args", List $ map String $ drop 1 args)]
  (runIOThrows $ liftM show $ eval env (List [Atom "load", String (args !! 0)])) >>= hPutStrLn stderr

runRepl :: IO ()
runRepl = primitiveBindings >>= until_ (== "quit") (readPrompt "Lisp>>> ") . evalAndPrint


main :: IO ()
main = do
  args <- getArgs
  if length args == 0 then runRepl else runOne $ args
