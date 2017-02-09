
module Target.Dynamics where


import Unbound.LocallyNameless
import Target.Syntax
import Common

------------------------
-- big-step evaluation
------------------------

eval :: Expr -> FreshM Expr
eval (App e1 e2) = do
  Lam t <- eval e1
  (x, body) <- unbind t
  eval (subst x e2 body)
eval (Lam t) = return (Lam t)
eval (BLam t) = return (BLam t)
eval (TApp e t) = do
  BLam b <- eval e
  (x, body) <- unbind b
  eval (subst x t body)
eval (Pair e1 e2) = do
  v1 <- eval e1
  v2 <- eval e2
  return (Pair v1 v2)
eval (Proj1 e) = do
  (Pair v1 v2) <- eval e
  return v1
eval (Proj2 e) = do
  (Pair v1 v2) <- eval e
  return v2
eval (IntV n) = return (IntV n)
eval (BoolV b) = return (BoolV b)
eval Unit = return Unit
eval (PrimOp op e1 e2) = do
  IntV v1 <- eval e1
  IntV v2 <- eval e2
  return (evalOp op v1 v2)
eval (If c e1 e2) = do
  BoolV t <- eval c
  if t then eval e1
    else eval e2

evaluate :: Expr -> Expr
evaluate = runFreshM . eval



evalOp :: Operation -> Int -> Int -> Expr
evalOp op x y =
  case op of
    (Arith Add) -> IntV $ x + y
    (Arith Sub) -> IntV $ x - y
    (Arith Mul) -> IntV $ x * y
    (Arith Div) -> IntV $ x `div` y
    (Logical Equ) -> BoolV $ x == y
    (Logical Neq) -> BoolV $ x /= y
    (Logical Lt) -> BoolV $ x < y
    (Logical Gt) -> BoolV $ x > y
