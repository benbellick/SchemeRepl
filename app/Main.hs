module Main (main) where

--import Lib

import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad
import Numeric

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
    x <- many (noneOf "\"\\" <|> parseEscapeChar)
    _ <- char '"'
    return $ String x

parseEscapeChar :: Parser Char
parseEscapeChar = do
    _ <- char '\\'
    x <- oneOf "\"nrt\\"
    return $ charToEscape x
    where charToEscape c = case c of 
            '\\' -> '\\'
            'n' -> '\n'
            'k' -> '\r'
            't' -> '\t'
            otherwise -> ' '

parseAtom :: Parser LispVal
parseAtom = do
    first <- letter <|> symbol
    rest  <- many (letter <|> digit <|> symbol)
    let atom = first:rest
    return $ case atom of
        "#t" -> Bool True
        "#f" -> Bool False
        _    -> Atom atom

parseDecNumber :: Parser LispVal
parseDecNumber = liftM (Number . read) $ many1 digit

parseNondecNumber :: Parser LispVal
parseNondecNumber = do
    _ <- char '#'
    b <- oneOf "bodx"
    n <- many1 digit
    return $ Number $ toNum b n
    where toNum base num = case base of
            'd' -> read num
            'o' -> fst . head $ readOct num {-- this seems unsafe --}
            'x' -> fst . head $ readHex num {-- this seems unsafe --}
            'b' -> fst . head $ readBin num {-- this seems unsafe --}

parseNumber :: Parser LispVal
parseNumber = parseDecNumber <|> parseNondecNumber

parseExpr :: Parser LispVal
parseExpr = parseNumber
        <|> parseString
        <|> parseAtom

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right (String s) -> "Found value (string): " ++ s
    Right (Number i) -> "Found value (int): " ++ show i
    Right (Atom a) -> "Found value (atom): " ++ a
    Right val -> "Found value (not string)"

main :: IO ()
main = do
    (expr:_) <- getArgs
    putStrLn $ readExpr expr
