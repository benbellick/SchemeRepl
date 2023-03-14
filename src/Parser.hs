module Parser (parseExpr, LispVal(..), spaces) where

import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad
import Numeric
import Data.Char
import Data.Complex
import Data.Ratio
import Types

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

parseChar :: Parser LispVal
parseChar = do
    _ <- char '#'
    _ <- try (char '\\')
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
extractInteger _ = error "only works with numbers"

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
extractFloat _ = error "Only works with float and number"

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
        <|> parseAtom
        <|> parseChar
        <|> parseString
        <|> parseQuoted
        <|> do 
            _ <- char '('
            x <- try parseList <|> parseDottedList
            _ <- char ')'
            return x

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
    h <- endBy parseExpr spaces
    t <- char '.' >> spaces >> parseExpr
    return $ DottedList h t

--Parse syntactic sugar of single quote for 
parseQuoted :: Parser LispVal
parseQuoted = do 
    _ <- char '\''
    x <- parseExpr
    return $ List [Atom "quote", x]

-- Maybe should be moved to Types module
showVal :: LispVal -> String
showVal (String s) = "\"" ++ s ++ "\""
showVal (Atom a ) = a
showVal (Number n) = show n
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List l) = "(" ++ unwordsList l ++ ")"
showVal (DottedList h t) = "(" ++ unwordsList h ++ " . " ++ showVal t ++ ")"
showVal (Rational r) = show r
showVal (Float f) = show f
showVal (Complex c) = show c
showVal (Char c) = show c
showVal (PrimitiveFunc _) = "<primitive>"
showVal (NonPrimFunc Func {params = args, vararg = varargs, body = body, closure = env}) =
  "(lambda (" ++ unwords (map show args) ++
    (case varargs of
       Nothing -> ""
       Just arg -> " . " ++ arg
    ) ++ ") ... )"
showVal (IOFunc _) = "<IO primitive>"
showVal (Port _) = "<IO port>"


unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

instance Show LispVal where show = showVal

