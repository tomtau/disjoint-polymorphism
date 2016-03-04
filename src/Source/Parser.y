{
module Source.Parser (parseExpr) where

import Data.Char (isDigit, isSpace, isAlpha)
import Data.List (stripPrefix)
import Source.Syntax
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


%nonassoc ',,' '&'
%right LAM LET
%right '->'
%left '+' '-'
%left '*'
%nonassoc ':'


%monad { Either String }

%%

expr : '\\' id '.' expr   %prec LAM       { elam $2 $4 }
     | expr ':' type                      { Anno $1 $3 }
     | aexp                               { $1 }
     | let id '=' expr in expr  %prec LET { App (elam $2 $6) $4 }
     | expr '+' expr                      { PrimOp Add $1 $3 }
     | expr '-' expr                      { PrimOp Sub $1 $3 }
     | expr '*' expr                      { PrimOp Mul $1 $3 }
     | expr ',,' expr                     { Merge $1 $3 }

aexp : aexp term                      { App $1 $2 }
     | term                           { $1 }

term : id                             { evar $1 }
     | intVal                         { IntV $1 }
     | boolVal                        { BoolV $1 }
     | '(' expr ')'                   { $2 }

type : int                            { IntT }
     | bool                           { BoolT }
     | type '&' type                  { Inter $1 $3 }
     | type '->' type                 { Arr $1 $3 }

{

parseError _ = Left "Parse error!"

parseExpr = parser . scanTokens

}
