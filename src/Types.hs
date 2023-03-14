module Types(LispVal(..), Func(..), LispError(..), ThrowsError, Env, nullEnv)  where

import Data.IORef
import Text.Parsec (ParseError)
import Numeric()
import Data.Char()
import Data.Complex
import Data.Ratio()
import System.IO
import Control.Monad.Except

type Env = IORef [(String, IORef LispVal)]

nullEnv :: IO Env
nullEnv = newIORef []

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
             | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
             | NonPrimFunc Func 
             | IOFunc ([LispVal] -> IOThrowsError LispVal)
             | Port Handle

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
showVal (NonPrimFunc Func {params = args, vararg = varargs}) =
  "(lambda (" ++ unwords (map show args) ++
    (case varargs of
       Nothing -> ""
       Just arg -> " . " ++ arg
    ) ++ ") ... )"
showVal (IOFunc _) = "<IO primitive>"
showVal (Port _) = "<IO port>"

instance Show LispVal where show = showVal

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

data Func = Func { params  :: [String]
                 , vararg  :: (Maybe String)
                 , body    :: [LispVal]
                 , closure :: Env }

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
type IOThrowsError = ExceptT LispError IO
