{-# LANGUAGE RecursiveDo #-}

module SEDEL.Target.CallByNeed
  ( evaluate
  , emptyEnv
  ) where



import           Control.Monad.Trans
import           Data.IORef
import qualified Data.Map.Strict as M
import           Unbound.LocallyNameless

import           SEDEL.Target.Syntax
import           SEDEL.Common


type Env = M.Map UName (IORef Thunk)

emptyEnv :: Env
emptyEnv = M.empty

type Thunk = () -> Mt Value

data Value = VLit Double
           | VBool Bool
           | VStr String
           | VPair Thunk Thunk
           | VUnit
           | VClosure (Thunk -> Mt Value)


instance Show Value where
  show (VLit n) = show n
  show (VBool True) = "true"
  show (VBool False) = "false"
  show (VPair p1 p2) = "<<pair>>"
  show VUnit = "()"
  show (VStr s) = show s
  show VClosure {} = "<<closure>>"

update :: IORef Thunk -> Value -> Mt ()
update ref v = do
  liftIO $ writeIORef ref (\() -> return v)
  return ()

force :: IORef Thunk -> Mt Value
force ref = do
  th <- liftIO $ readIORef ref
  v <- th ()
  update ref v
  return v

mkThunk :: Env -> UName -> UExpr -> (Thunk -> Mt Value)
mkThunk env x body a = do
  a' <- liftIO $ newIORef a
  eval (M.insert x a' env) body

lookupEnv :: Env -> UName -> Mt (IORef Thunk)
lookupEnv env y = case M.lookup y env of
  Nothing -> error $ "Unbound Variable" ++ name2String y
  Just th -> return th

type Mt = FreshMT IO

evaluate :: UExpr -> IO Value
evaluate e = runFreshMT (eval emptyEnv e)

eval :: Env -> UExpr  -> Mt Value
eval env e = case e of
  (UVar x) -> do
    th <- lookupEnv env x
    force th
  (UApp e1 e2) -> do
    VClosure c <- eval env e1
    c (\() -> eval env e2)
  (ULam b) -> do
    (x, body) <- unbind b
    return $ VClosure (mkThunk env x body)
  (ULet b) -> mdo
    (x, (e', body)) <- unbind b
    -- recursive let binding
    let arg () = eval env' e'
    a <- liftIO $ newIORef arg
    let env' = M.insert x a env
    eval env' body
  (UPair e1 e2) -> do
    let p1 () = eval env e1
        p2 () = eval env e2
    return $ VPair p1 p2
  (UP1 p) -> do
    VPair p1 _ <- eval env p
    p1 ()
  (UP2 p) -> do
    VPair _ p2 <- eval env p
    p2 ()
  (ULitV n) -> return $ VLit n
  (UBoolV n) -> return $ VBool n
  (UStrV n) -> return $ VStr n
  UUnit -> return VUnit
  (UPrimOp op e1 e2) -> do
    v1 <- eval env e1
    v2 <- eval env e2
    return $ evalOp op v1 v2
  (UIf e1 e2 e3) -> do
    (VBool v) <- eval env e1
    if v then eval env e2 else eval env e3
  (UToString e') -> do
    v <- eval env e'
    return $ VStr (show v)
  (USqrt e') -> do
    (VLit n) <- eval env e'
    return $ VLit (sqrt n)
  Bot -> error "Evaluation would not terminate"


evalOp :: Operation -> Value -> Value -> Value
evalOp (Arith Add) (VLit x) (VLit y) = VLit $ x + y
evalOp (Arith Sub) (VLit x) (VLit y) = VLit $ x - y
evalOp (Arith Mul) (VLit x) (VLit y) = VLit $ x * y
evalOp (Arith Div) (VLit x) (VLit y) = VLit $ x / y
evalOp (Comp Equ) (VLit x) (VLit y) = VBool $ x == y
evalOp (Comp Equ) (VStr x) (VStr y) = VBool $ x == y
evalOp (Comp Equ) (VBool x) (VBool y) = VBool $ x == y
evalOp (Comp Lt) (VLit x) (VLit y) = VBool $ x < y
evalOp (Comp Gt) (VLit x) (VLit y) = VBool $ x > y
evalOp (Comp Neq) (VLit x) (VLit y) = VBool $ x /= y
evalOp (Comp Neq) (VStr x) (VStr y) = VBool $ x /= y
evalOp (Comp Neq) (VBool x) (VBool y) = VBool $ x /= y
evalOp (Logical LAnd) (VBool x) (VBool y) = VBool $ x && y
evalOp (Logical LOr) (VBool x) (VBool y) = VBool $ x || y
evalOp Append (VStr x) (VStr y) = VStr $ x ++ y
evalOp _ _ _ = error "Impossible happened in evalOp"
