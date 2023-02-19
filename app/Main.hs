module Main (main) where

--import Lib

import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad
import Numeric
import Data.Char
import Data.Complex
import Data.Ratio

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | Rational Rational
             | Float Float
             | Complex (Complex Float)
             | Char Char
             | String String
             | Bool Bool

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

parseChar :: Parser LispVal
parseChar = do
    _ <- char '#'
    _ <- char '\\'
    x <- many (noneOf " \t\n\r") {--refactor?--}
    return $ charLitToChar x
            
charLitToChar :: String -> LispVal
charLitToChar cs = Char 
    (case (map toLower cs) of
        "space" -> ' '
        "newline" -> '\n'
        _ -> head cs) {--throw error when more than one extra??--}

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
            _ -> ' ' {-- is this an error?--}

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

extractInteger :: LispVal -> Integer
extractInteger (Number n) = n

parseRational :: Parser LispVal
parseRational = do
    x <- parseDecNumber
    _ <- char '/'
    y <- parseDecNumber
    return . Rational $ (extractInteger x) % (extractInteger y)

parseFloat :: Parser LispVal
parseFloat = do
    x <- many1 digit
    _ <- char '.'
    y <- many1 digit
    return $ Float $ (fst . head . readFloat) $ x ++ "." ++ y

extractFloat :: LispVal -> Float
extractFloat (Float f) = f
extractFloat (Number i) = fromInteger i

parseComplex :: Parser LispVal
parseComplex = do
    x <- parseFloat <|> parseDecNumber
    _ <- char '+'
    y <- parseFloat <|> parseDecNumber
    _ <- char 'i'
    return $ Complex $ (extractFloat x) :+ (extractFloat y)

parseNumber :: Parser LispVal
parseNumber = try parseComplex 
          <|> try parseFloat 
          <|> try parseRational 
          <|> try parseNondecNumber 
          <|> parseDecNumber 

parseExpr :: Parser LispVal
parseExpr = parseNumber
        <|> try parseChar
        <|> try parseString
        <|> try parseAtom

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
    Right _val -> "Found value (not string)"

main :: IO ()
main = do
    (expr:_) <- getArgs
    putStrLn $ readExpr expr
