
module Target.CBN
  ( Env
  , evaluate
  , extendCtx
  , emptyEnv
  ) where


import Common
import Control.Monad.Reader
import Target.Syntax
import Unbound.LocallyNameless
import PrettyPrint
import qualified Data.Map.Strict as M

data ClosureExp = CExp UExpr Env deriving Show

type Env = M.Map UName ClosureExp

emptyEnv = M.empty

extendCtx :: (UName, UExpr, Env) -> Env -> Env
extendCtx (n, e, env) env' = M.insert n (CExp e env) env'

data Value = VInt Int
           | VBool Bool
           | VPair ClosureExp ClosureExp
           | VUnit
           | VClosure (Bind UName UExpr) Env

instance Show Value where
  show (VInt n) = show n
  show (VBool n) = show n
  show (VPair (CExp e1 _) (CExp e2 _)) = "(" ++ pprint e1 ++ ", " ++ pprint e2 ++ ")"
  show VUnit = "()"
  show _ = "Cannot show functions"

type EvalMonad = FreshMT (Reader Env)


evaluate :: Env -> UExpr -> Value
evaluate env e = runReader (runFreshMT (eval e)) env


eval :: UExpr -> EvalMonad Value
eval (UVar x) = do
  Just (CExp e env) <- asks (M.lookup x)
  local (const env) $ eval e
eval (UApp e1 e2) = do
  VClosure b env' <- eval e1
  (x, body) <- unbind b
  env <- ask
  local (const $ M.insert x (CExp e2 env) env') $ (eval body)
eval (ULam b) = do
  env <- ask
  return $ VClosure b env
eval (ULet b) = do
  (x, (e, body)) <- unbind b
  env <- ask
  let env' = M.insert x (CExp e env') env -- Recursive let binding
  local (M.insert x (CExp e env')) $ eval body
eval (UPair e1 e2) = do
  env <- ask
  return $ VPair (CExp e1 env) (CExp e2 env)
eval (UP1 e) = do
  VPair (CExp e1 env') _ <- eval e
  v1 <- local (const env') $ eval e1
  return v1
eval (UP2 e) = do
  VPair _ (CExp e2 env') <- eval e
  v2 <- local (const env') $ eval e2
  return v2
eval (UIntV n) = return $ VInt n
eval (UBoolV n) = return $ VBool n
eval UUnit = return VUnit
eval (UPrimOp op e1 e2) = do
  (VInt v1) <- eval e1
  (VInt v2) <- eval e2
  return $ evalOp op v1 v2
eval (UIf e1 e2 e3) = do
  (VBool v) <- eval e1
  if v then eval e2 else eval e3


evalOp :: Operation -> Int -> Int -> Value
evalOp op x y =
  case op of
    (Arith Add) -> VInt $ x + y
    (Arith Sub) -> VInt $ x - y
    (Arith Mul) -> VInt $ x * y
    (Arith Div) -> VInt $ x `div` y
    (Logical Equ) -> VBool $ x == y
    (Logical Neq) -> VBool $ x /= y
    (Logical Lt) -> VBool $ x < y
    (Logical Gt) -> VBool $ x > y
