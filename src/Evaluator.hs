{-# LANGUAGE ExistentialQuantification #-}
module Evaluator (eval, primitiveBindings) where

import Types
import EnvManage
import Control.Monad.Except
import System.IO
import Reader

eval :: Env -> LispVal -> IOThrowsError LispVal
eval _env val@(String _) = return val
eval _penv val@(Number _) = return val
eval _env val@(Rational _) = return val
eval _env val@(Float _) = return val
eval _env val@(Complex _) = return val
eval _env val@(Char _) = return val
eval _env val@(Bool _) = return val
eval  env (Atom var) = getVar env var
eval  env (List [Atom "load", String filename]) = load filename >>= liftM last . mapM (eval env)
eval _env (List [Atom "quote", val]) = return val
eval  env (List [Atom "if", cond, consq, alt]) = do
  result <- eval env cond
  case result of
    Bool True -> eval env consq
    Bool False -> eval env alt
    _ -> throwError $ TypeMismatch "bool" cond
eval  env (List (Atom "cond":xs)) = cond env xs
eval  env (List (Atom "case":x:xs)) = eval env x >>= caseSch env xs
eval  env (List [Atom "set!", Atom var, form]) = eval env form >>= setVar env var
eval  env (List [Atom "define", Atom var, form]) = eval env form >>= defineVar env var
eval  env (List (Atom "define" : List (Atom var : params) : body)) = makeNormalFunc env params body >>= defineVar env var
eval  env (List (Atom "define" : DottedList (Atom var : params) varargs : body)) = makeVarArgs varargs env params body >>= defineVar env var
eval  env (List (Atom "lambda" : List params : body)) = makeNormalFunc env params body
eval  env (List (Atom "lambda" : DottedList params varargs : body)) = makeVarArgs varargs env params body
eval  env (List (Atom "lambda" : varargs@(Atom _) : body)) = makeVarArgs varargs env [] body
eval  env (List (func : args)) = do {f <- eval env func; argVals <- mapM (eval env) args; apply f argVals;}
eval _env badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

makeFunc :: Maybe String -> Env -> [LispVal] -> [LispVal] -> IOThrowsError LispVal
makeFunc varArgs env params body = return $ NonPrimFunc $ Func (map show params) varArgs body env
makeNormalFunc = makeFunc Nothing
makeVarArgs :: LispVal -> Env -> [LispVal] -> [LispVal] -> IOThrowsError LispVal
makeVarArgs = makeFunc . Just . show

apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
apply (PrimitiveFunc func) args = liftThrows $ func args
apply (NonPrimFunc ( Func params varargs body closure)) args =
  if num params /= num args && varargs == Nothing
  then throwError $ NumArgs (num params) args
  else (liftIO $ bindVars closure $ zip params args) >>= bindVarArgs varargs >>= evalBody
  where num = toInteger . length
        remainingArgs = drop (length params) args
        evalBody env = liftM last $ mapM (eval env) body
        bindVarArgs arg env = case arg of
          Just argName -> liftIO $ bindVars env [(argName, List $ remainingArgs)]
          Nothing -> return env
apply (IOFunc f) args = f args

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [
              ("=", numBoolBinop (==)),
              ("<", numBoolBinop (<)),
              (">", numBoolBinop (>)),
              ("/=", numBoolBinop (/=)),
              (">=", numBoolBinop (>=)),
              ("<=", numBoolBinop (<=)),
              ("&&", boolBoolBinop (&&)),
              ("||", boolBoolBinop (||)),
              ("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem),
              ("symbol?", unaryOp isSymbol),
              ("string?", unaryOp isString),
              ("number?", unaryOp isNumber),
              ("bool?", unaryOp isBool),
              ("symbol->string",  symbolToString),
              ("string->symbol",  stringToSymbol),
              ("string=?", strBoolBinop (==)),
              ("string<?", strBoolBinop (<)),
              ("string>?", strBoolBinop (>)),
              ("string<=?", strBoolBinop (<=)),
              ("string>=?", strBoolBinop (>=)),
              ("car", car),
              ("cdr", cdr),
              ("cons", cons),
              ("eq?", eqv),
              ("eqv?", eqv),
              ("equal?", equal)
              ]

ioPrimitives :: [(String, [LispVal] -> IOThrowsError LispVal)]
ioPrimitives = [("apply", applyProc),
                ("open-input-file", makePort ReadMode),
                ("open-output-file", makePort WriteMode),
                ("close-input-port", closePort),
                ("close-output-port", closePort),
                ("read", readProc),
                ("write", writeProc),
                ("read-contents", readContents),
                ("read-all", readAll)
               ]

primitiveBindings :: IO Env
primitiveBindings = nullEnv >>= (flip bindVars $ map (makeFunc IOFunc)       ioPrimitives ++
                                                 map (makeFunc PrimitiveFunc) primitives)
                    where makeFunc cstr (var, func) = (var, cstr func)

applyProc :: [LispVal] -> IOThrowsError LispVal
applyProc [func, List args] = apply func args
applyProc (func : args) = apply func args
applyProc v = throwError $ NumArgs 1 v

makePort :: IOMode -> [LispVal] -> IOThrowsError LispVal
makePort mode [String filename] = liftM Port $ liftIO $ openFile filename mode
makePort _    [v] = throwError $ TypeMismatch "expected file name" v
makePort _    v   = throwError $ NumArgs 1 v

closePort :: [LispVal] -> IOThrowsError LispVal
closePort [Port port] = liftIO $ hClose port >> (return $ Bool True)
closePort _           = return $ Bool False

readProc :: [LispVal] -> IOThrowsError LispVal
readProc [] = readProc [Port stdin]
readProc [Port port] = (liftIO $ hGetLine port) >>= liftThrows . readExpr
readProc [v] = throwError $ TypeMismatch "expected Port" v
readProc v@(_:_:_) = throwError $ NumArgs 1 v

writeProc :: [LispVal] -> IOThrowsError LispVal
writeProc [obj] = writeProc [obj, Port stdout]
writeProc [obj, Port port] = liftIO $ hPrint port obj >> (return $ Bool True)
writeProc v@(_:_:_:_) = throwError $ NumArgs 1 v {- extend numargs to handle multiple? -}
writeProc [] = throwError $ NumArgs 1 []
writeProc [_, badPort] = throwError $ TypeMismatch "expected second arg of type Port" badPort

readContents :: [LispVal] -> IOThrowsError LispVal
readContents [String filename] = liftM String $ liftIO $ readFile filename
readContents [badArg] = throwError $ TypeMismatch "expected single file name" badArg
readContents v@[] = throwError $ NumArgs 1 v
readContents v@(_:_:_) = throwError $ NumArgs 1 v 

load :: String -> IOThrowsError [LispVal]
load filename = (liftIO $ readFile filename) >>= liftThrows . readExprList

readAll :: [LispVal] -> IOThrowsError LispVal
readAll [String filename] = liftM List $ load filename
readAll [badArg] = throwError $ TypeMismatch "expected single file name" badArg
readAll v@[] = throwError $ NumArgs 1 v
readAll v@(_:_:_) = throwError $ NumArgs 1 v 

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop _ [] = throwError $ NumArgs 2 []
numericBinop _ val@[_] = throwError $ NumArgs 2 val
numericBinop op params = mapM unpackNum params >>= return . Number . foldl1 op

unaryOp :: (LispVal -> LispVal) -> [LispVal] -> ThrowsError LispVal
unaryOp _ [] = throwError $ NumArgs 1 []
unaryOp op [val] = return $ op val
unaryOp _ val = throwError $ NumArgs 1 val

boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op args = if length args /= 2 
                             then throwError $ NumArgs 2 args
                             else do
                                      left <- unpacker $ args !! 0
                                      right <- unpacker $ args !! 1
                                      return $ Bool $ left `op` right
numBoolBinop :: (Integer -> Integer -> Bool) -> [LispVal] -> ThrowsError LispVal
numBoolBinop  = boolBinop unpackNum
strBoolBinop :: (String -> String -> Bool) -> [LispVal] -> ThrowsError LispVal
strBoolBinop  = boolBinop unpackStr
boolBoolBinop :: (Bool -> Bool -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBoolBinop = boolBinop unpackBool

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
--unpackNum (Rational n) = n
--unpackNum (Float n) = n
--unpackNum (Complex n) = n
unpackNum (String n) = let parsed = reads n :: [(Integer, String)] in --only reads integer, not gen number
    if null parsed
        then throwError $ TypeMismatch "number" $ String n
        else return $ fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum notNum = throwError $ TypeMismatch "number" notNum

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Number s) = return $ show s
unpackStr (Bool s)   = return $ show s
unpackStr notString  = throwError $ TypeMismatch "string" notString

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool  = throwError $ TypeMismatch "boolean" notBool

isSymbol, isString, isNumber, isBool :: LispVal -> LispVal
isSymbol (Atom _) = Bool True
isSymbol _ = Bool False

isString (String _) = Bool True
isString _ = Bool False

isNumber (Number _) = Bool True
isNumber _ = Bool False

isBool (Bool _) = Bool True
isBool _ = Bool False

symbolToString, stringToSymbol :: [LispVal] -> ThrowsError LispVal
symbolToString [Atom a] = return . String $ a
symbolToString [notAtom] = throwError $ TypeMismatch "symbol" notAtom
symbolToString val = throwError $ NumArgs 1 val

stringToSymbol [String s] = return . Atom $ s
stringToSymbol [notString] = throwError $ TypeMismatch "string" notString
stringToSymbol val = throwError $ NumArgs 1 val


-- List Primitives
car :: [LispVal] -> ThrowsError LispVal
car [List (x:_)] = return x
car [DottedList (x:_) _] = return x
car [badArg] = throwError $ TypeMismatch "pair" badArg
car badArgList = throwError $ NumArgs 1 badArgList

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (_:xs)] = return . List $ xs
cdr [DottedList [_] x] = return x
cdr [DottedList (_:xs) x] = return $ DottedList xs x
cdr [badArg] = throwError $ TypeMismatch "pair" badArg
cdr badArgList = throwError $ NumArgs 1 badArgList

cons :: [LispVal] -> ThrowsError LispVal
cons [x1, List []] = return $ List [x1]
cons [x, List xs] = return $ List $ x : xs
cons [x, DottedList xs xlast] = return $ DottedList (x:xs) xlast
cons [x1, x2] = return $ DottedList [x1] x2
cons badArgList = throwError $ NumArgs 2 badArgList

eqv :: [LispVal] -> ThrowsError LispVal
eqv [(Bool arg1), (Bool arg2)]             = return $ Bool $ arg1 == arg2
eqv [(Number arg1), (Number arg2)]         = return $ Bool $ arg1 == arg2
eqv [(String arg1), (String arg2)]         = return $ Bool $ arg1 == arg2
eqv [(Atom arg1), (Atom arg2)]             = return $ Bool $ arg1 == arg2
eqv [(DottedList xs x), (DottedList ys y)] = eqv [List $ xs ++ [x], List $ ys ++ [y]]
eqv [(List arg1), (List arg2)]             = return $ Bool $ (length arg1 == length arg2) && 
                                                             (all eqvPair $ zip arg1 arg2)
     where eqvPair (x1, x2) = case eqv [x1, x2] of
                                Left _err -> False
                                Right (Bool val) -> val
                                Right _ -> False {- really should throw error -}
eqv [_, _]                                 = return $ Bool False
eqv badArgList                             = throwError $ NumArgs 2 badArgList

data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)

unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals arg1 arg2 (AnyUnpacker unpacker) = 
             do unpacked1 <- unpacker arg1
                unpacked2 <- unpacker arg2
                return $ unpacked1 == unpacked2
        `catchError` (const $ return False)

equal :: [LispVal] -> ThrowsError LispVal
equal [(List arg1), (List arg2)] = return $ Bool $ (length arg1 == length arg2) &&
                                                   (all eqvPair $ zip arg1 arg2)
         where eqvPair (x1, x2) = case equal [x1, x2] of
                                    Left _err -> False
                                    Right (Bool val) -> val
                                    Right _ -> False {- really should throw error -}
                                    
equal [arg1, arg2] = do
      primitiveEquals <- liftM or $ mapM (unpackEquals arg1 arg2) 
                         [AnyUnpacker unpackNum, AnyUnpacker unpackStr, AnyUnpacker unpackBool]
      eqvEquals <- eqv [arg1, arg2]
      return $ Bool $ (primitiveEquals || let (Bool x) = eqvEquals in x)
equal badArgList = throwError $ NumArgs 2 badArgList

-- Later refactor, cond is just caseSch with #t instead of targetValue
cond :: Env -> [LispVal] -> IOThrowsError LispVal
cond _env [] = throwError $ Default "No viable alternative in cond"
cond env [List ((Atom "else"):xs)] = do
  results <- mapM (eval env) xs
  return $ last results
cond env ((List x):xs) = do
  result <- (eval env) . head $ x
  case result of
    Bool True -> do
      evals <- mapM (eval env) $ tail x
      return $ last evals
    Bool False -> cond env xs
    badArg -> throwError $ TypeMismatch "boolean" badArg
cond _env badArg = throwError $ TypeMismatch "list of lists" $ List badArg

caseSch :: Env -> [LispVal] -> LispVal -> IOThrowsError LispVal
caseSch _ [] _ = throwError $ Default "No viable alternative in case"
caseSch env [List ((Atom "else"):xs)] _ = do
  results <- mapM (eval env) xs
  return $ last results

caseSch env (x:xs) targetVal = do
  e <- liftThrows $ eqv [targetVal, x]
  case e of
    Bool False -> caseSch env xs targetVal
    Bool True -> do
      evals <- mapM (eval env) $ tail xs
      return $ last evals
    _ -> throwError $ Default "Implementation error: eqv should only return #t or #f"
    

