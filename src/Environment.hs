module Environment(
  Env,
  nullEnv,
  IOThrowsError,
  liftThrows,
  runIOThrows,
  isBound,
  getVar,
  setVar,
  defineVar,
  bindVars
) where

import Parser
import Error
import Control.Monad.Except
import Data.IORef

type Env = IORef [(String, IORef LispVal)]

nullEnv :: IO Env
nullEnv = newIORef []

type IOThrowsError = ExceptT LispError IO

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = runExceptT (trapError action) >>= return . extractValue

isBound :: Env -> String -> IO Bool
isBound envRef var = readIORef envRef >>= return . maybe False (const True) . lookup var

getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef var = do env <- liftIO $ readIORef envRef
                       maybe (throwError $ UnboundVar "Getting an unbound variable" var)
                             (liftIO . readIORef)
                             (lookup var env)

setVar :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar envRef var val = do env <- liftIO $ readIORef envRef
                           maybe (throwError $ UnboundVar "Setting an unbound variable" var)
                                 writeValToVar
                                 (lookup var env)
                           return val
                           where writeValToVar = liftIO . (flip writeIORef val)

defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal
defineVar envRef var val = do
  alreadyDefined <- liftIO $ isBound envRef var
  if alreadyDefined
    then setVar envRef var val >> return val
    else liftIO $ do
         valRef <- newIORef val
         env <- readIORef envRef
         writeIORef envRef ((var, valRef) : env)
         return val
                    
bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
   where extendEnv bindings env = liftM (++ env) (mapM addBinding bindings)
         addBinding (var, val) = do {ref <- newIORef val; return (var, ref);}
