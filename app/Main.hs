import Parser
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right (String s) -> "Found value (string): " ++ s
    Right (Number i) -> "Found value (int): " ++ show i
    Right (Atom a) -> "Found value (atom): " ++ a
    Right (Char c) -> "Found value (Char): " ++ [c]
    Right (Float f) -> "Found value (Float): " ++ show f
    Right (Complex c) -> "Found value (Complex): " ++ show c
    Right (Rational r) -> "Found value (Rational): " ++ show r
    Right (List l ) -> "Found value (List)"
    Right _val -> "Found value (not string)"

main :: IO ()
main = do
    (expr:_) <- getArgs
    putStrLn $ readExpr expr
