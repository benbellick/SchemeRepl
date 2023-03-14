module Error(trapError, extractValue) where

import Types
import Control.Monad.Except

showError :: LispError -> String
showError (UnboundVar msg varname) = msg ++ ": " ++ varname
showError (BadSpecialForm msg form) = msg ++ ": " ++ show form
showError (NotFunction msg func) = msg ++ ": " ++ show func
showError (NumArgs expected found) =  "Expected " ++ show expected ++ " args; found values: " ++ show found
showError (TypeMismatch expected found) =  "Invalid type: expected " ++ expected ++ ", found " ++ show found
showError (Parser parseErr) = "Parse error at " ++ show parseErr
showError (Default msg) = "Default error: " ++ msg

instance Show LispError where show = showError

trapError :: (MonadError e m, Show e) => m String -> m String
trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right x) = x
extractValue (Left  x) = error $ show x
