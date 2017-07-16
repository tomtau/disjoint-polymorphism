module SEDEL.Target.Malfunction
  (generateMalfunction)
  where

import SEDEL.Target.Syntax
import SEDEL.Common
import Unbound.LocallyNameless
import qualified SEDEL.Target.CBN as C
import qualified SEDEL.Source.Syntax as ST
import qualified Data.Map.Strict as M

data Sexp = S [Sexp] | A String | KInt Int | KStr String

instance Show Sexp where
  show s = render s "" where
    render (S s') k = "(" ++ foldr render (") " ++ k) s'
    render (A s') k = s' ++ " " ++ k
    render (KInt n) k = show n ++ " " ++ k
    render (KStr s') k = show s' ++ " " ++ k

emitDecls :: C.Env -> M [Sexp]
emitDecls env =
  sequence decls
  where
    envL = M.toList env
    decls = map emitD envL
    emitD (k, C.CExp v env') = do
      e <- emitExp v
      return $ S [A ("$" ++ show k), e]

emit :: C.Env -> ST.Type -> UExpr -> M [Sexp]
emit env t e = do
  decls <- emitDecls env
  sexp <- emitExp e
  let mexp = case t of
        ST.NumT -> S [A "apply", S [A "global", A "$Pervasives", A "$print_int"], sexp]
        ST.StringT -> S [A "apply", S [A "global", A "$Pervasives", A "$print_string"], sexp]
        ST.BoolT -> S [A "apply", S [A "global", A "$Pervasives", A "$print_bool"], sexp]
        _ -> sexp
  return $ (decls ++ [S [A "_", mexp]])
  where

generateMalfunction :: C.Env -> ST.Type -> UExpr -> Sexp
generateMalfunction env t e =
  let sexps = runFreshM (emit env t e) in
  S (A "module" : (sexps ++ [S [A "export"]]))

type M = FreshM

emitExp :: UExpr -> M Sexp
emitExp (ULitV d) = return $ KInt (round d)
emitExp (UBoolV True) = return $ KInt 1
emitExp (UBoolV False) = return $ KInt 0
emitExp (UStrV s) = return $ KStr s
emitExp UUnit = return $ KInt 0
emitExp (UPrimOp op e1 e2) = emitOp op e1 e2
emitExp (UVar u) = return $ A ("$" ++ show u)
emitExp (UApp e1 e2) =
  do e1' <- emitExp e1
     e2' <- emitExp e2
     return $ S [A "apply", e1', e2']
emitExp (ULam b) =
  do (x, body) <- unbind b
     b' <- emitExp body
     return $ S [A "lambda", S [A ("$" ++ show x)], b']
emitExp (ULet b) =
  do (x, (e, body)) <- unbind b
     e' <- emitExp e
     b' <- emitExp body
     return $ S [A "let", S [A "rec", S [A ("$" ++ show x), e']],
                 S [A "$res", b'], A "$res"]
emitExp (UPair e1 e2) =
  do e1' <- emitExp e1
     e2' <- emitExp e2
     return $ S [A "block", S [A "tag", KInt 0, e1', e2']]
emitExp (UP1 e) =
  do e' <- emitExp e
     return $ S [A "field", KInt 1, e'] 
emitExp (UP2 e) =
  do e' <- emitExp e
     return $ S [A "field", KInt 2, e']
emitExp (UIf c e1 e2) =
  do c' <- emitExp c
     e2' <- emitExp e2
     e1' <- emitExp e1
     return $ S [A "switch", c',
                 S [KInt 0, e2'],
                 S [A "_", S [A "tag", A "_"], e1']]
emitExp (UToString u) =
  do u' <- emitExp u
     return $ S [A "apply", S [A "global", A "$Pervasives", A "string_of_int"], u']
emitExp (USqrt u) =
  do u' <- emitExp u
     return $ S [A "apply", S [A "global", A "$Pervasives", A "sqrt"], u']
emitExp Bot = return $ S []

returnOp :: UExpr -> UExpr -> String -> M Sexp
returnOp e1 e2 op = 
  do e1' <- emitExp e1
     e2' <- emitExp e2
     return $ S [A op, e1', e2']
     
emitOp :: Operation -> UExpr -> UExpr -> M Sexp
emitOp (Arith Add) e1 e2 = returnOp e1 e2 "+"
emitOp (Arith Sub) e1 e2 = returnOp e1 e2 "-"
emitOp (Arith Mul) e1 e2 = returnOp e1 e2 "*"
emitOp (Arith Div) e1 e2 = returnOp e1 e2 "/"
emitOp (Comp Lt) e1 e2 = returnOp e1 e2 "<"
emitOp (Comp Gt) e1 e2 = returnOp e1 e2 ">"
emitOp (Comp Equ) e1 e2 = returnOp e1 e2 "=="
emitOp (Comp Neq) e1 e2 = returnOp e1 e2 "!="
emitOp (Logical LAnd) e1 e2 = returnOp e1 e2 "&&"
emitOp (Logical LOr) e1 e2 = returnOp e1 e2 "||"
emitOp Append e1 e2 =
  do e1' <- emitExp e1
     e2' <- emitExp e2
     return $ S [A "apply", S [A "global", A "$Pervasives", A "$^"], e1', e2' ]
