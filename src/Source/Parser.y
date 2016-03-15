{
module Source.Parser (parseExpr) where

import Data.Char (isDigit, isSpace, isAlpha)
import Data.List (stripPrefix)
import Source.Syntax
import Common
import Tokens
}


%name parser
%tokentype { Token }
%error { parseError }


%token
    let      { TLet }
    in       { TIn }
    int      { TIntTy }
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
    '*'      { TMult }
    '\\'     { TLam }
    '+'      { TPlus }
    '-'      { TMinus }
    '&'      { TAnd }
    ',,'     { TMerge }
    '._'     { TProj }
    top      { TTop }


%nonassoc ',,' '&' '*'
%right LAM LET
%right '->'
%nonassoc IF
%left '+' '-'
%left '._'
%nonassoc ':'


%monad { Either String }

%%

expr : '\\' id '.' expr   %prec LAM          { elam $2 $4 }
     | expr ':' type                         { Anno $1 $3 }
     | aexp                                  { $1 }
     | let id '=' expr in expr  %prec LET    { Let $ ebindt ($2, $4) $6 }
     | expr '+' expr                         { PrimOp Add $1 $3 }
     | expr '-' expr                         { PrimOp Sub $1 $3 }
     | expr ',,' expr                        { Merge $1 $3 }
     | '(' expr ',' expr ')'                 { Pair $2 $4 }
     | expr '._' intVal                      { Project $1 $3 }
     | if expr then expr else expr  %prec IF { If $2 $4 $6 }

aexp : aexp term                      { App $1 $2 }
     | term                           { $1 }

term : id                             { evar $1 }
     | intVal                         { IntV $1 }
     | boolVal                        { BoolV $1 }
     | top                            { Top }
     | '(' expr ')'                   { $2 }

type : int                            { IntT }
     | bool                           { BoolT }
     | type '&' type                  { Inter $1 $3 }
     | type '*' type                  { Product $1 $3 }
     | type '->' type                 { Arr $1 $3 }
     | '(' type ')'                   { $2 }
     | top                            { TopT }

{

parseError _ = Left "Parse error!"

parseExpr = parser . scanTokens

}
