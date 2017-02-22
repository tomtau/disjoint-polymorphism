{
module Source.Parser (parseExpr) where

import Data.Char (isDigit, isSpace, isAlpha)
import Data.List (stripPrefix)
import Source.Syntax
import Unbound.LocallyNameless
import Common
import Lexer
import PrettyPrint
import Source.SrcLoc
}


%name parser
%tokentype { Token }
%monad { Alex } { (>>=) } { return }
%lexer { lexer } { T _ TEOF "" }
%error { parseError }


%token

    def    { T _ TKey "def" }
    defrec { T _ TKey "defrec" }
    typ    { T _ TKey "type" }
    let    { T _ TKey "let" }
    in     { T _ TKey "in" }
    module { T _ TKey "module" }
    int    { T _ TKey "int" }
    bool   { T _ TKey "bool" }
    boolV  { T _ (TBool $$) _ }
    id     { T _ (TId $$) _ }
    num    { T _ (TInt $$) _ }
    string { T _ TKey "string" }
    str    { T _ (TStr $$) _ }
    if     { T _ TKey "if" }
    then   { T _ TKey "then" }
    else   { T _ TKey "else" }
    ':'    { T _ TSym ":" }
    '='    { T _ TSym "=" }
    '.'    { T _ TSym "." }
    ','    { T _ TSym "," }
    '{'    { T _ TSym "{" }
    '}'    { T _ TSym "}" }
    '['    { T _ TSym "[" }
    ']'    { T _ TSym "]" }
    '->'   { T _ TSym "->" }
    '('    { T _ TSym "(" }
    ')'    { T _ TSym ")" }
    lam    { T _ TSym "\\" }
    forall { T _ TSym "\\/" }
    blam   { T _ TSym "/\\" }
    '&'    { T _ TSym "&" }
    ',,'   { T _ TSym ",," }
    top    { T _ TSym "T" }
    '+'    { T _ TSym "+" }
    '-'    { T _ TSym "-" }
    '*'    { T _ TSym "*" }
    '/'    { T _ TSym "/" }
    '<'    { T _ TSym "<" }
    '>'    { T _ TSym ">" }
    '=='   { T _ TSym "==" }
    '/='   { T _ TSym "/=" }
    '@'    { T _ TSym "@" }
    ';'    { T _ TSym ";" }



%right LAM LET DLAM FIX FORALL
%right '->'
%left ',,' '&'
%nonassoc IF
%nonassoc '==' '/='
%nonassoc '<' '>'
%left '@'
%left '+' '-'
%left '*' '/'
%nonassoc ':'
%left '.'



%%

prog : decllist expr_or_unit   { Module $1 $2 }


decllist :: { [Decl] }
decllist : {- empty -}  { [] }
         | decl ';' decllist { $1 : $3}

expr_or_unit :: { Expr }
expr_or_unit : expr { $1 }
             | {- empty -} { Top }

decl :: { Decl }
decl : def id teleidlst lteleidlst ':' type '=' expr
              { let (typ, trm) = teleToTmBind $3 $4 $6 $8
                in TmDef (s2n $2) typ trm }
     | typ id teleidlst '=' type     { TyDef (s2n $2) TopT (teleToBind $3 $5) }
     | defrec id teleidlst lteleidlst ':' type '=' expr
              { let (typ, trm) = teleToTmBind $3 $4 $6 $8
                in TmDef (s2n $2) typ (elet $2 typ trm (evar $2))   }


teleidlst :: { [(String, Type)] }
teleidlst : {- empty -}  { [] }
          | teleid teleidlst  { $1 : $2 }

teleid :: { (String, Type) }
teleid : tele { $1 }
       | id { ($1, TopT) }

tele :: { (String, Type) }
tele : '[' id '*' type ']'    { ($2, $4) }

lteleidlst :: { [(String, Type)] }
lteleidlst : {- empty -}  { [] }
           | lamtele lteleidlst  { $1 : $2 }

lamtele :: { (String, Type) }
lamtele : '(' id ':' type ')' { ($2, $4) }


expr :: { Expr }
expr : lam id '.' expr   %prec LAM                { elam $2 $4 }
     | blam id '*' type '.' expr %prec DLAM       { dlam $2 $4 $6 }
     | blam id '.' expr %prec DLAM                { dlam $2 TopT $4 }
     | expr ':' type                              { Anno $1 $3 }
     | '{' recds '}'                              { mkRecds $2 }
     | let id ':' type '=' expr in expr %prec LET { elet $2 $4 $6 $8 }
     | expr '+' expr                              { PrimOp (Arith Add) $1 $3 }
     | expr '-' expr                              { PrimOp (Arith Sub) $1 $3 }
     | expr '*' expr                              { PrimOp (Arith Mul) $1 $3 }
     | expr '/' expr                              { PrimOp (Arith Div) $1 $3 }
     | expr '==' expr                             { PrimOp (Logical Equ) $1 $3 }
     | expr '/=' expr                             { PrimOp (Logical Neq) $1 $3 }
     | expr '<' expr                              { PrimOp (Logical Lt) $1 $3 }
     | expr '>' expr                              { PrimOp (Logical Gt) $1 $3 }
     | expr ',,' expr                             { Merge $1 $3 }
     | if expr then expr else expr  %prec IF      { If $2 $4 $6 }
     | aexp                                       { $1 }

recds :: { [(String, Expr)] }
recds : recd                 { [$1] }
      | recd ',' recds       { $1 : $3 }

recd : id '=' expr          { ($1, $3) }

aexp :: { Expr }
aexp : aexp term                                { App $1 $2 }
     | aexp '@' type                            { TApp $1 $3 }
     | aexp '.' id                                { Acc $1 $3 }
     | term                                     { $1 }

term :: { Expr }
term : num                                   { IntV $1 }
     | boolV                                  { BoolV $1 }
     | str                                    { StrV $1 }
     | id                                       { evar $1 }
     | top                                      { Top }
     | '(' expr ')'                             { $2 }

type :: { Type }
type : int                                      { IntT }
     | bool                                     { BoolT }
     | string                                   { StringT }
     | type '&' type                            { And $1 $3 }
     | type '->' type                           { Arr $1 $3 }
     | '{' recdsT '}'                           { mkRecdsT $2 }
     | '(' type ')'                             { $2 }
     | forall id '*' type '.' type %prec FORALL { tforall $2 $4 $6 }
     | forall id '.' type %prec FORALL          { tforall $2 TopT $4 }
     | top                                      { TopT }
     | id                                       { tvar $1 }

recdsT :: { [(String, Type)] }
recdsT : recdT                 { [$1] }
      | recdT ',' recdsT       { $1 : $3 }

recdT : id ':' type          { ($1, $3) }

{

parseError :: Token -> Alex a
parseError (T p _ s) =
  alexError (showPosn p ++ ": parse error at token '" ++ s ++ "'")

parseExpr :: String -> Either String Module
parseExpr inp = runAlex inp parser

}
