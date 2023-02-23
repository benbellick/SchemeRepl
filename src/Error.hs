module Error(LispError(..), ThrowsError, trapError, extractValue) where

import Parser (LispVal)
import Text.Parsec (ParseError)
import Control.Monad.Except

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String


showError :: LispError -> String
showError (UnboundVar msg varname) = msg ++ ": " ++ varname
showError (BadSpecialForm msg form) = msg ++ ": " ++ show form
showError (NotFunction msg func) = msg ++ ": " ++ show func
showError (NumArgs expected found) =  "Expected " ++ show expected ++ " args; found values: " ++ show found
showError (TypeMismatch expected found) =  "Invalid type: expected " ++ expected ++ ", found " ++ show found
showError (Parser parseErr) = "Parse error at " ++ show parseErr
showError (Default msg) = "Default error: " ++ msg

instance Show LispError where show = showError

type ThrowsError = Either LispError

trapError :: ThrowsError String -> ThrowsError String
trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right x) = x
