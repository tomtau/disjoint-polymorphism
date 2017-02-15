{
module Source.Parser (parseExpr) where

import Data.Char (isDigit, isSpace, isAlpha)
import Data.List (stripPrefix)
import Source.Syntax
import           Unbound.LocallyNameless
import Common
import Tokens
import PrettyPrint
import qualified Data.Text as T
}


%name parser
%tokentype { Token }
%error { parseError }


%token
    'let'      { TLet }
    'in'       { TIn }
    'int'      { TIntTy }
    'fix'      { TFix }
    'bool'     { TBoolTy }
    boolVal  { TBool $$ }
    id       { TStr $$ }
    intVal   { TInt $$ }
    'if'       { TIf }
    'then'     { TThen }
    'else'     { TElse }
    ':'      { TColon }
    '='      { TEq }
    '#'      { TSharp }
    '.'      { TDot }
    ','      { TComma }
    '['      { TLSquare }
    ']'      { TRSquare }
    '{'      { TLCurly }
    '}'      { TRCurly }
    '->'     { TArr }
    '('      { TLParen }
    ')'      { TRParen }
    '\\'     { TLam }
    '\\\/'   { TForall }
    '\/\\'   { TDLam }
    '&'      { TAnd }
    ',,'     { TMerge }
    'top'      { TTop }
    '+'      { TAdd }
    '-'      { TSub }
    '*'      { TMul }
    '/'      { TDiv }
    '<'      { TLt }
    '>'      { TGt }
    '=='     { TEqu }
    '!='     { TNeq }
    '@'      { TAt }


%right LAM LET DLAM FIX FORALL
%right '->'
%nonassoc ',,' '&'
%nonassoc IF
%nonassoc '==' '!='
%nonassoc '<' '>'
%left '@'
%left '+' '-'
%left '*' '/'
%nonassoc ':'
%left '.'



%monad { Either T.Text }

%%

expr :: { Expr }
expr : '\\' id '.' expr   %prec LAM           { elam $2 $4 }
     | '\/\\' id '*' type '.' expr %prec DLAM { dlam $2 $4 $6 }
     | '\/\\' id '.' expr %prec DLAM          { dlam $2 TopT $4 }
     -- | 'fix' id '.' expr    %prec FIX           { efix $2 $4 }
     | expr ':' type                          { Anno $1 $3 }
     | '{' recds '}'                          { mkRecds $2 }
     -- | 'let' id ':' type '=' expr 'in' expr %prec LET       { }
     | expr '+' expr                          { PrimOp (Arith Add) $1 $3 }
     | expr '-' expr                          { PrimOp (Arith Sub) $1 $3 }
     | expr '*' expr                          { PrimOp (Arith Mul) $1 $3 }
     | expr '/' expr                          { PrimOp (Arith Div) $1 $3 }
     | expr '==' expr                         { PrimOp (Logical Equ) $1 $3 }
     | expr '!=' expr                         { PrimOp (Logical Neq) $1 $3 }
     | expr '<' expr                          { PrimOp (Logical Lt) $1 $3 }
     | expr '>' expr                          { PrimOp (Logical Gt) $1 $3 }
     | expr ',,' expr                         { Merge $1 $3 }
     | expr '.' id                            { Acc $1 $3 }
     | 'if' expr 'then' expr 'else' expr  %prec IF  { If $2 $4 $6 }
     | aexp                                   { $1 }

recds :: { [(String, Expr)] }
recds : recd                 { [$1] }
      | recd ',' recds       { $1 : $3 }

recd : id '=' expr          { ($1, $3) }

aexp :: { Expr }
aexp : aexp term                                { App $1 $2 }
     | aexp '@' type                            { TApp $1 $3 }
     | term                                     { $1 }

term :: { Expr }
term : id                                       { evar $1 }
     | intVal                                   { IntV $1 }
     | boolVal                                  { BoolV $1 }
     | 'top'                                      { Top }
     | '(' expr ')'                             { $2 }

type :: { Type }
type : 'int'                                      { IntT }
     | 'bool'                                     { BoolT }
     | type '&' type                            { And $1 $3 }
     | type '->' type                           { Arr $1 $3 }
     | '{' recdsT '}'                           { mkRecdsT $2 }
     | '(' type ')'                             { $2 }
     | '\\\/' id '*' type '.' type %prec FORALL { tforall $2 $4 $6 }
     | '\\\/' id '.' type %prec FORALL          { tforall $2 TopT $4 }
     | 'top'                                      { TopT }
     | id                                       { tvar $1 }

recdsT :: { [(String, Type)] }
recdsT : recdT                 { [$1] }
      | recdT ',' recdsT       { $1 : $3 }

recdT : id ':' type          { ($1, $3) }

{

evar :: String -> Expr
evar = Var . s2n

tvar :: String -> Type
tvar = TVar . s2n

ebind :: String -> Expr -> Bind TmName Expr
ebind n = bind (s2n n)

elam :: String -> Expr -> Expr
elam b e = Lam (ebind b e)

-- efix :: String -> Expr -> Expr
-- efix b e = FixP (ebind b e)

dlam :: String -> Type -> Expr -> Expr
dlam s t b = DLam (bind (s2n s, embed t) b)

tforall :: String -> Type -> Type -> Type
tforall s t b = DForall (bind (s2n s, embed t) b)

eapp :: Expr -> Expr -> Expr
eapp = App

etapp :: Expr -> Type -> Expr
etapp = TApp

mkRecds :: [(Label, Expr)] -> Expr
mkRecds [(l, e)] = DRec l e
mkRecds ((l, e) : r) = Merge (DRec l e) (mkRecds r)

mkRecdsT :: [(Label, Type)] -> Type
mkRecdsT [(l, e)] = SRecT l e
mkRecdsT ((l, e) : r) = And (SRecT l e) (mkRecdsT r)


parseError t = Left . T.pack $ "Cannot parse: " ++ show t

parseExpr = parser . scanTokens

}
