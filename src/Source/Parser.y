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

    def         { T _ TKey "def" }
    defrec      { T _ TKey "defrec" }
    trait       { T _ TKey "trait" }
    Trait       { T _ TKey "Trait" }
    new         { T _ TKey "new" }
    forall      { T _ TKey "forall" }
    typ         { T _ TKey "type" }
    let         { T _ TKey "let" }
    in          { T _ TKey "in" }
    module      { T _ TKey "module" }
    int         { T _ TKey "Int" }
    bool        { T _ TKey "Bool" }
    boolV       { T _ (TBool $$) _ }
    num         { T _ (TInt $$) _ }
    string      { T _ TKey "String" }
    str         { T _ (TStr $$) _ }
    if          { T _ TKey "if" }
    then        { T _ TKey "then" }
    else        { T _ TKey "else" }
    ':'         { T _ TSym ":" }
    '='         { T _ TSym "=" }
    '.'         { T _ TSym "." }
    ','         { T _ TSym "," }
    '{'         { T _ TSym "{" }
    '}'         { T _ TSym "}" }
    '['         { T _ TSym "[" }
    ']'         { T _ TSym "]" }
    '->'        { T _ TSym "->" }
    '('         { T _ TSym "(" }
    ')'         { T _ TSym ")" }
    lam         { T _ TSym "\\" }
    blam        { T _ TSym "/\\" }
    '&'         { T _ TSym "&" }
    ',,'        { T _ TSym ",," }
    top         { T _ TSym "()" }
    topT        { T _ TSym "T" }
    '+'         { T _ TSym "+" }
    '-'         { T _ TSym "-" }
    '*'         { T _ TSym "*" }
    '/'         { T _ TSym "/" }
    '<'         { T _ TSym "<" }
    '>'         { T _ TSym ">" }
    '=='        { T _ TSym "==" }
    '/='        { T _ TSym "/=" }
    ';'         { T _ TSym ";" }
    '_'         { T _ TSym "_" }
    '=>'        { T _ TSym "=>" }
    '++'        { T _ TSym "++" }


    LOWER_IDENT { T _ (Tlowerid $$) _ }
    UPPER_IDENT { T _ (Tupperid $$) _ }

%right LAM LET DLAM FIX FORALL
%right '->'
%left ',,' '&'
%nonassoc IF
%nonassoc '==' '/='
%nonassoc '<' '>'
%left '+' '-' '++'
%left '*' '/'
%nonassoc ':'
%left '.'



%%




prog :: { Module }
prog : decllist expr_or_unit   { Module $1 $2 }

traitdecl :: { Trait }
traitdecl : trait LOWER_IDENT params '{' LOWER_IDENT ':' type '=>' sdecllist '}'
                  { TraitDef $2 ($5, $7) (bind (map (\(n, b) -> (s2n n, embed b)) $3) $9) }

decllist :: { [Decl] }
decllist : {- empty -}       { [] }
         | decl ';' decllist { $1 : $3}

decl :: { Decl }
decl : sdecl     { SDecl $1 }
     | traitdecl { TraitDecl $1 }

expr_or_unit :: { Expr }
expr_or_unit : expr        { $1 }
             | {- empty -} { Top }


sdecllist :: { [SimpleDecl] }
sdecllist : {- empty -}         { [] }
          | sdecl ';' sdecllist { $1 : $3 }


sdecl :: { SimpleDecl }
sdecl : def LOWER_IDENT teleidlst lteleidlst ':' type '=' expr
              { let (typ, trm) = teleToTmBind $3 $4 $6 $8
                in TmDef $2 typ trm }
      | typ UPPER_IDENT teleidlst '=' type     { TyDef $2 TopT (teleToBind $3 $5) }
      | defrec LOWER_IDENT teleidlst lteleidlst ':' type '=' expr
              { let (typ, trm) = teleToTmBind $3 $4 $6 $8
                in TmDef $2 typ (elet $2 typ trm (evar $2))   }


teleidlst :: { [(String, Type)] }
teleidlst : {- empty -}       { [] }
          | teleid teleidlst  { $1 : $2 }

teleid :: { (String, Type) }
teleid : tele        { $1 }
       | UPPER_IDENT { ($1, TopT) }

tele :: { (String, Type) }
tele : '[' UPPER_IDENT '*' type ']'    { ($2, $4) }

lteleidlst :: { [(String, Type)] }
lteleidlst : {- empty -}         { [] }
           | lamtele lteleidlst  { $1 : $2 }

lamtele :: { (String, Type) }
lamtele : '(' LOWER_IDENT ':' type ')' { ($2, $4) }
        |  top                         { ("_", TopT) }

params :: { [(String, Type)] }
params : top                   { [("_", TopT)] }
       | '(' pairs ')'         { $2 }


traitConstrs :: { [Expr] }
traitConstrs : traitConstr                      { [$1] }
             | traitConstr '&' traitConstrs     { $1:$3 }


traitConstr :: { Expr }
traitConstr : LOWER_IDENT args     { App (foldl App (evar $1) $2) (evar "self") }


-- args can be
--  (x, y, z)
--  ()
--  none
args :: { [Expr] }
args : {- empty -}        { [] }
     | top                { [Top] }
     | '(' arglist ')'    { $2 }

arglist :: { [Expr] }
arglist : expr                 { [$1] }
        | expr ',' arglist       { $1:$3 }


expr :: { Expr }
expr : lam LOWER_IDENT '->' expr   %prec LAM               { elam $2 $4 }
     | lam '_' '->' expr   %prec LAM                       { elam "_" $4 }
     | blam UPPER_IDENT '*' type '.' expr %prec DLAM       { dlam $2 $4 $6 }
     | blam UPPER_IDENT '.' expr %prec DLAM                { dlam $2 TopT $4 }
     | expr ':' type                                       { Anno $1 $3 }
     | '{' recds '}'                                       { mkRecds $2 }
     | let LOWER_IDENT ':' type '=' expr in expr %prec LET { elet $2 $4 $6 $8 }
     | expr '+' expr                                       { PrimOp (Arith Add) $1 $3 }
     | expr '-' expr                                       { PrimOp (Arith Sub) $1 $3 }
     | expr '*' expr                                       { PrimOp (Arith Mul) $1 $3 }
     | expr '/' expr                                       { PrimOp (Arith Div) $1 $3 }
     | expr '==' expr                                      { PrimOp (Logical Equ) $1 $3 }
     | expr '/=' expr                                      { PrimOp (Logical Neq) $1 $3 }
     | expr '<' expr                                       { PrimOp (Logical Lt) $1 $3 }
     | expr '>' expr                                       { PrimOp (Logical Gt) $1 $3 }
     | expr '++' expr                                      { PrimOp Append $1 $3 }
     | expr ',,' expr                                      { Merge $1 $3 }
     | if expr then expr else expr  %prec IF               { If $2 $4 $6 }
     | new '[' type ']' traitConstrs                       { transNew $3 $5 }
     | aexp                                                { $1 }

recds :: { [(String, Expr)] }
recds : recd                 { [$1] }
      | recd ',' recds       { $1 : $3 }

recd : LOWER_IDENT '=' expr          { ($1, $3) }

aexp :: { Expr }
aexp : aexp term            { App $1 $2 }
     | aexp type            { TApp $1 $2 }
     | aexp '.' LOWER_IDENT { Acc $1 $3 }
     | term                 { $1 }

term :: { Expr }
term : num          { IntV $1 }
     | boolV        { BoolV $1 }
     | str          { StrV $1 }
     | LOWER_IDENT  { evar $1 }
     | top          { Top }
     | '(' expr ')' { $2 }

type :: { Type }
type : int                                               { IntT }
     | bool                                              { BoolT }
     | string                                            { StringT }
     | type '&' type                                     { And $1 $3 }
     | type '->' type                                    { Arr $1 $3 }
     | '{' pairs '}'                                    { mkRecdsT $2 }
     | '(' type ')'                                      { $2 }
     | forall UPPER_IDENT '*' type '.' type %prec FORALL { tforall $2 $4 $6 }
     | forall UPPER_IDENT '.' type %prec FORALL          { tforall $2 TopT $4 }
     | topT                                              { TopT }
     | Trait '[' type ',' type ']'                       { Arr $3 $5 }
     | UPPER_IDENT                                       { tvar $1 }

pairs :: { [(String, Type)] }
pairs : pair    { [$1] }
      | pair ',' pairs { $1:$3 }


pair : LOWER_IDENT ':' type  { ($1, $3) }




{

parseError :: Token -> Alex a
parseError (T p _ s) =
  alexError (showPosn p ++ ": parse error at token '" ++ s ++ "'")

parseExpr :: String -> Either String Module
parseExpr inp = runAlex inp parser

}
