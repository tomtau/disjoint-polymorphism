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

%right '->'
%left ',,'
%nonassoc '==' '/='
%nonassoc '<' '>'
%left '+' '-' '++'
%left '*' '/'



%%


------------------------------------------------------------------------
-- Programs
------------------------------------------------------------------------

prog :: { Module }
  : decllist expr_or_unit   { Module $1 $2 }

traitdecl :: { Trait }
  : trait LOWER_IDENT trait_params_list '{' LOWER_IDENT ':' type '=>' sdecllist '}'
  { TraitDef $2 ($5, $7) (bind (map (\(n, b) -> (s2n n, embed b)) $3) $9) }

decllist :: { [Decl] }
  : {- empty -}       { [] }
  | decl ';' decllist { $1 : $3}

decl :: { Decl }
  : sdecl     { SDecl $1 }
  | traitdecl { TraitDecl $1 }

expr_or_unit :: { Expr }
  : expr        { $1 }
  | {- empty -} { Top }


sdecllist :: { [SimpleDecl] }
  : {- empty -}         { [] }
  | sdecl ';' sdecllist { $1 : $3 }


sdecl :: { SimpleDecl }
  : def bind                  { DefDecl $2 }
  | typ typebind              { TypeDecl $2 }
  | defrec recbind            { DefDecl $2 }


bind :: { TmBind }
  : LOWER_IDENT ctyparam_list param_list ':' type '=' expr
  { TmBind $1 (map (\(n, b) -> (s2n n, b)) $2) (map (\(n, b) -> (s2n n, b)) $3) $7 (Just $5) }
  | LOWER_IDENT ctyparam_list param_list '=' expr
  { TmBind $1 (map (\(n, b) -> (s2n n, b)) $2) (map (\(n, b) -> (s2n n, b)) $3) $5 Nothing }

recbind :: { TmBind }
  : LOWER_IDENT ctyparam_list param_list ':' type '=' expr
  { let (typ, trm) = teleToTmBind $2 $3 $5 $7
    in TmBind $1 [] [] (elet $1 typ trm (evar $1)) (Just typ)
  }


typebind :: { TypeBind }
  : UPPER_IDENT  typaram_list '=' type     { TypeBind $1 $2 $4 }

traitConstrs :: { [Expr] }
  : traitConstr                      { [$1] }
  | traitConstr '&' traitConstrs     { $1:$3 }


traitConstr :: { Expr }
  : LOWER_IDENT args                  { App (foldl App (evar $1) $2) (evar "self") }


-- Parse "(x, y, z)" or "() or nothing at all
args :: { [Expr] }
  : {- empty -}        { [] }
  | top                { [Top] }
  | '(' arglist ')'    { $2 }

arglist :: { [Expr] }
  : expr                   { [$1] }
  | expr ',' arglist       { $1:$3 }


-- Parse "(x : Int, y : Bool)" or "()" or nothing at all
trait_params_list :: { [(String, Type)] }
  : {- empty -}           { [] }
  | top                   { [("_", TopT)] }
  | '(' tparams ')'         { $2 }

tparams :: { [(String, Type)] }
      : tparam                 { [$1] }
      | tparam ',' tparams       { $1:$3 }

tparam : LOWER_IDENT ':' type  { ($1, $3) }



------------------------------------------------------------------------
-- Types
------------------------------------------------------------------------

type :: { Type }
  : forall ctyparams1 '.' type  { foldr tforall (tforall (last $2) $4) (init $2) }
  | monotype                     { $1 }

monotype :: { Type }
  : intertype '->' monotype  { Arr $1 $3 }
  | intertype                { $1 }

intertype :: { Type }
  : ftype '&' intertype      { And $1 $3 }
  | ftype                    { $1 }

ftype :: { Type }
  : ftype type_list  { foldl OpApp (OpApp $1 (head $2)) (tail $2) }
  | atype            { $1 }

type_list :: { [Type] }
  : '[' comma_types1 ']'     { $2 }

comma_types1 :: { [Type] }
  : type                   { [$1]  }
  | type ',' comma_types1  { $1:$3 }

atype :: { Type }
  : UPPER_IDENT                   { tvar $1 }
  | int                           { IntT }
  | bool                          { BoolT }
  | string                        { StringT }
  | topT                          { TopT }
  | record_type                   { $1 }
  | '(' type ')'                  { $2 }
  | Trait '[' type ',' type ']'   { Arr $3 $5 }

-- record types
record_type :: { Type }
  -- TODO: desugaring might be too early. But the benefit is avoid a traversal of the type structure later.
  : '{' record_type_fields_rev '}'      { mkRecdsT (reverse $2) }

-- Happy is more efficient at parsing left-recursive rules!
record_type_fields_rev :: { [(Label, Type)] }
  : record_type_field                             { [$1]  }
  | record_type_fields_rev ',' record_type_field  { $3:$1 }

record_type_field :: { (Label, Type) }
  : label ':' type                                { ($1, $3) }


{-------------------------------------------------------------------------------
        Type parameters
-------------------------------------------------------------------------------}

typaram :: { TyName }
  : UPPER_IDENT                    { s2n $1 }

typarams :: { [TyName] }
  : {- empty -}                    { []    }
  | typaram typarams               { $1:$2 }

typarams1 :: { [TyName] }
  : typaram typarams               { $1:$2 }

typaram_list :: { [TyName] }
  : '[' comma_typarams1 ']'        { $2 }
  | {- empty -}                    { [] }

comma_typarams1 :: { [TyName] }
  : typaram                        { [$1]  }
  | typaram ',' comma_typarams1    { $1:$3 }


{-------------------------------------------------------------------------------
        constrainable type parameters
-------------------------------------------------------------------------------}

ctyparam :: { (String, Type) }
  : UPPER_IDENT              { ($1, TopT) }
  | UPPER_IDENT '*' type     { ($1, $3) }
  | '(' ctyparam ')'         { $2 }

comma_ctyparams1 :: { [(String, Type)] }
  : ctyparam                       { [$1]  }
  | ctyparam ',' comma_ctyparams1  { $1:$3 }

ctyparam_list :: { [(String, Type)] }
  : '[' comma_ctyparams1 ']'       { $2 }
  | {- empty -}                    { [] }

paren_ctyparam :: { (String, Type) }
  : UPPER_IDENT                     { ($1, TopT) }
  | '(' UPPER_IDENT '*' type ')'    { ($2, $4) }

ctyparams :: { [(String, Type)] }
  : {- empty -}                     { []    }
  | paren_ctyparam ctyparams        { $1:$2 }

ctyparams1 :: { [(String, Type)] }
  : paren_ctyparam ctyparams        { $1:$2 }


------------------------------------------------------------------------
-- Expressions
------------------------------------------------------------------------

expr :: { Expr }
     : lam lparam_list1 '->' expr                            { foldr elam (elam (last $2) $4) (init $2) }
     | blam ctyparams1 '.' expr                             { foldr dlam (dlam (last $2) $4) (init $2) }
     | let LOWER_IDENT ':' type '=' expr in expr   { elet $2 $4 $6 $8 }
     | if expr then expr else expr                  { If $2 $4 $6 }
     | new '[' type ']' traitConstrs                        { transNew $3 $5 }
     | infixexpr                                            { $1 }

infixexpr :: { Expr }
          : infixexpr '+' infixexpr                                       { PrimOp (Arith Add) $1 $3 }
          | infixexpr '-' infixexpr                                       { PrimOp (Arith Sub) $1 $3 }
          | infixexpr '*' infixexpr                                       { PrimOp (Arith Mul) $1 $3 }
          | infixexpr '/' infixexpr                                       { PrimOp (Arith Div) $1 $3 }
          | infixexpr '==' infixexpr                                      { PrimOp (Logical Equ) $1 $3 }
          | infixexpr '/=' infixexpr                                      { PrimOp (Logical Neq) $1 $3 }
          | infixexpr '<' infixexpr                                       { PrimOp (Logical Lt) $1 $3 }
          | infixexpr '>' infixexpr                                       { PrimOp (Logical Gt) $1 $3 }
          | infixexpr '++' infixexpr                                      { PrimOp Append $1 $3 }
          | infixexpr ',,' infixexpr                                      { Merge $1 $3 }
          | fexpr                                                         { $1 }


fexpr :: { Expr }
      : fexpr aexpr      { App  $1 $2 }
      | fexpr type       { TApp $1 $2 }
      | aexpr            { $1 }

aexpr :: { Expr }
      : LOWER_IDENT                      { evar $1 }
      | num                              { IntV $1 }
      | boolV                            { BoolV $1 }
      | str                              { StrV $1 }
      | aexpr '.' LOWER_IDENT            { Acc $1 $3 }
      | record_construct                 { $1 }
      | top                              { Top }
      | aexpr ':' type                   { Anno $1 $3 }
      | '(' expr ')'                     { $2 }

-- record-construction expr
record_construct :: { Expr }
  : '{' record_construct_fields_rev '}'                     { mkRecds (reverse $2) }

record_construct_fields_rev :: { [(Label, Expr)] }
  : record_construct_field                                  { [$1]  }
  | record_construct_fields_rev ',' record_construct_field  { $3:$1 }

record_construct_field :: { (Label, Expr) }
  : label '=' expr                                          { ($1, $3) }


-- Parse "(x : Int) (y : Int)" or "()" or nothing at all
param_list :: { [(String, Type)] }
  : params                    { $1 }
  | top                       { [("_", TopT)] }

param_list1 :: { [(String, Type)] }
  : params1                   { $1 }

param :: { (String, Type) }
  : '(' LOWER_IDENT ':' type ')' { ($2, $4) }

params :: { [(String, Type)] }
  : {- empty -}  { []    }
  | param params { $1:$2 }

params1 :: { [(String, Type)] }
  : param params { $1:$2 }


-- Parse "x y z" or noting at all
lparam_list :: { [String] }
  : lparams                    { $1 }

lparam_list1 :: { [String] }
  : lparams1                   { $1 }

lparam :: { String }
  : LOWER_IDENT  { $1 }
  | '_'           { "_" }

lparams :: { [String] }
  : {- empty -}  { []    }
  | lparam lparams { $1:$2 }

lparams1 :: { [String] }
  : lparam lparams { $1:$2 }


-- Misc
------------------------------------------------------------------------

ident :: { String }
  : UPPER_IDENT  { $1 }
  | LOWER_IDENT  { $1 }

label :: { Label }
  : LOWER_IDENT  { $1 }


{

parseError :: Token -> Alex a
parseError (T p _ s) =
  alexError (showPosn p ++ ": parse error at token '" ++ s ++ "'")

parseExpr :: String -> Either String Module
parseExpr inp = runAlex inp parser

}
