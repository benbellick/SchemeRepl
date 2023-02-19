import Parser
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> show val

main :: IO ()
main = do
    (expr:_) <- getArgs
    putStrLn $ readExpr expr
