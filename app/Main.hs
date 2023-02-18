module Main (main) where

--import Lib

import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

parseString :: Parser LispVal
parseString = do
    _ <- char '"'
    x <- many (noneOf "\"\\" <|> (string "\\\"" >> return '"'))
    _ <- char '"'
    return $ String x

parseEscapeChar :: Parser Char
parseEscapeChar = 
    (string "\\\"" >> return '"')
    --(string "\\n" >> return '\n')
    --(string "\\\\" >> return '\\' )
    --(string "\\r" >> return '\r') <|>
    --(string "\\t" >> return '\t') <|>

parseAtom :: Parser LispVal
parseAtom = do
    first <- letter <|> symbol
    rest  <- many (letter <|> digit <|> symbol)
    let atom = first:rest
    return $ case atom of
        "#t" -> Bool True
        "#f" -> Bool False
        _    -> Atom atom

parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) $ many1 digit
{--parseNumber = do
    d <- many1 digit
    return . Number . read $ d
--}
--parseNumber = many1 digit >>= (\ d -> return . Number . read $ d)

parseExpr :: Parser LispVal
parseExpr = parseAtom
        <|> parseString
        <|> parseNumber

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right (String s) -> "Found value: " ++ s
    Right val -> "Found value (not string)"

main :: IO ()
main = do
    (expr:_) <- getArgs
    putStrLn $ readExpr expr
