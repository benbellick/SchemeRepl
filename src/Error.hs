module Error(trapError, extractValue) where

import Types
import Control.Monad.Except

trapError :: (MonadError e m, Show e) => m String -> m String
trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right x) = x
extractValue (Left  x) = error $ show x
