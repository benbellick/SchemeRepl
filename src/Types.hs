module Types(LispVal(..), LispError(..), ThrowsError, Env, nullEnv)  where

import Data.IORef
import Text.Parsec (ParseError)
import Numeric
import Data.Char
import Data.Complex
import Data.Ratio

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
             | Func { params  :: [String]
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

type ThrowsError = Either LispError
