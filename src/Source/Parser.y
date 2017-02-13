{
module Source.Parser (parseExpr) where

import Data.Char (isDigit, isSpace, isAlpha)
import Data.List (stripPrefix)
import Source.Syntax
import Common
import Tokens
import PrettyPrint
import qualified Data.Text as T
}


%name parser
%tokentype { Token }
%error { parseError }


%token
    let      { TLet }
    in       { TIn }
    int      { TIntTy }
    fix      { TFix }
    bool     { TBoolTy }
    boolVal  { TBool $$ }
    id       { TStr $$ }
    intVal   { TInt $$ }
    if       { TIf }
    then     { TThen }
    else     { TElse }
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
    '._'     { TProj }
    top      { TTop }
    '+'      { TAdd }
    '-'      { TSub }
    '*'      { TMul }
    '/'      { TDiv }
    '<'      { TLt }
    '>'      { TGt }
    '=='     { TEqu }
    '!='     { TNeq }
    '@'      { TAt }


%right FORALL
%right LAM LET DLAM FIX
%right '->'
%nonassoc ',,' '&'
%nonassoc IF
%nonassoc '==' '!='
%nonassoc '<' '>'
%left '@'
%left '+' '-'
%left '*' '/'
%nonassoc ':'


%monad { Either T.Text }

%%

expr :: { Expr }
expr : '\\' id '.' expr   %prec LAM           { elam $2 $4 }
     | '\/\\' id '*' type '.' expr %prec DLAM { dlam $2 $4 $6 }
     | '\/\\' id '.' expr %prec DLAM          { dlam $2 TopT $4 }
     | fix id '.' expr    %prec FIX           { efix $2 $4 }
     | expr ':' type                          { Anno $1 $3 }
     | aexp                                   { $1 }
     | expr '+' expr                          { PrimOp (Arith Add) $1 $3 }
     | expr '-' expr                          { PrimOp (Arith Sub) $1 $3 }
     | expr '*' expr                          { PrimOp (Arith Mul) $1 $3 }
     | expr '/' expr                          { PrimOp (Arith Div) $1 $3 }
     | expr '==' expr                         { PrimOp (Logical Equ) $1 $3 }
     | expr '!=' expr                         { PrimOp (Logical Neq) $1 $3 }
     | expr '<' expr                          { PrimOp (Logical Lt) $1 $3 }
     | expr '>' expr                          { PrimOp (Logical Gt) $1 $3 }
     | expr ',,' expr                         { Merge $1 $3 }
     | if expr then expr else expr  %prec IF  { If $2 $4 $6 }

aexp :: { Expr }
aexp : aexp term                                { App $1 $2 }
     | aexp '@' type                            { TApp $1 $3 }
     | term                                     { $1 }

term :: { Expr }
term : id                                       { evar $1 }
     | intVal                                   { IntV $1 }
     | boolVal                                  { BoolV $1 }
     | top                                      { Top }
     | '(' expr ')'                             { $2 }

type :: { Type }
type : int                                      { IntT }
     | bool                                     { BoolT }
     | type '&' type                            { And $1 $3 }
     | type '->' type                           { Arr $1 $3 }
     | '(' type ')'                             { $2 }
     | '\\\/' id '*' type '.' type %prec FORALL { tforall $2 $4 $6 }
     | '\\\/' id '.' type %prec FORALL          { tforall $2 TopT $4 }
     | top                                      { TopT }
     | id                                       { tvar $1 }

{

parseError t = Left . T.pack $ "Cannot parse: " ++ show t

parseExpr = parser . scanTokens

}
