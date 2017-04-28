--> "add1(var x = 3.0; var y = 4.0; (if (x < y) then (x + 2.0) else (y + 3.0))) = 6.0"


-----------------------------
-- Infrastructure
-----------------------------

type MaybeAlg[B, A] = {
  nothing : B,
  just : A -> B
}

type Maybe[A] = {
   match : forall C. MaybeAlg[C,A] -> C
}

def nothing A : Maybe[A] = { match = /\C. \f -> f.nothing }

def just A (x : A) : Maybe[A] = { match = /\C. \f -> f.just x }

def bind A B (x : Maybe[A]) (f : A -> Maybe[B]) : Maybe[B] =
  x.match Maybe[B] { nothing = nothing B, just = \a -> f a }

def fromJust A (x : Maybe[A]) : A =
  x.match A { nothing = undefined, just = \b -> b }

def isJust A (x : Maybe[A]) : Bool =
  x.match Bool { nothing = false, just = \_ -> true }

type EnvF[E] = String -> Maybe[E]

def empty A : EnvF[A] = \_ -> {match = /\C. \f -> f.nothing}

def lookup A (s : String) (env : EnvF[A]) = env s

def insert A (s1 : String) (v1 : A) (f : EnvF[A]) : EnvF[A] =
  \s2 -> if s1 == s2 then just A v1 else f s2


type PairAlg[A,B,C] = {
  pairAlg : A -> B -> C
}

type Pair[A,B] = {
  match : forall C. PairAlg[A,B,C] -> C
}


def mkPair A B (x : A) (y : B) : Pair[A,B] = {
  match = /\C. \f -> f.pairAlg x y
}

def fst A B (p : Pair[A,B]) : A = p.match A { pairAlg = \x _ -> x }

def snd A B (p : Pair[A,B]) : B = p.match B { pairAlg = \_ x -> x }

def casePair A B C (p : Pair[A,B]) (f: A -> B -> C) : C =
  p.match C { pairAlg = \a b -> f a b }



-----------------------------
-- Values
-----------------------------

type ValAlg[E] = {
  numV : Double -> E,
  boolV : Bool -> E
}
type Value = {
  match : forall C. ValAlg[C] -> C
}

def numV (n : Double) : Value = { match = /\C. \f -> f.numV n }

def boolV (b : Bool) : Value = { match = /\C. \f -> f.boolV b }

def fromNum (v : Value) : Double =
  v.match Double { numV = \n -> n, boolV = \_ -> undefined}

def fromBool (v : Value) : Bool =
  v.match Bool { numV = \_ -> undefined, boolV = \b -> b }


-----------------------------
-- Types
-----------------------------

type TypeAlg[E] = {
  tnum : E,
  tbool : E
}
type Type = {
  match : forall E. TypeAlg[E] -> E
}

def tnum : Type = { match = /\E. \f -> f.tnum }

def tbool : Type = { match = /\E. \f -> f.tbool }

def caseType A (t : Type) (x : A) (y : A) : A =
  t.match A {tnum = x, tbool = y}

def eqTypes (a : Type) (b : Type) : Bool =
  caseType Bool a (caseType Bool b true false) (caseType Bool b false true)



----------------
-- Interfaces
----------------

-- Evaluator
type Env = EnvF[Value]
type IEval = {eval : Env -> Maybe[Value]}

-- Pretty printer
type IPrint = { print : String }

-- Type checker
type TEnv = EnvF[Type]
type ITC = { tcheck : TEnv -> Maybe[Type] }




--------------------------------------
-- Features
--------------------------------------


----------------
-- nat feature
----------------

type NatAlg[E] = {
  num : Double -> E,
  add : E -> E -> E,
  sub : E -> E -> E,
  mul : E -> E -> E,
  div : E -> E -> E
}

-- Evaluator algebra
trait evalNatAlg { self =>
  def num (n : Double) : IEval = { eval = \_ -> just Value (numV n) }
  def add (e1 : IEval) (e2 : IEval) : IEval = { eval = \env ->
    bind Value Value (e1.eval env) (\v1 ->
      bind Value Value (e2.eval env) (\v2 ->
        just Value (numV (fromNum v1 + fromNum v2))))
  }
  def sub (e1 : IEval) (e2 : IEval) : IEval = { eval = \env ->
    bind Value Value (e1.eval env) (\v1 ->
      bind Value Value (e2.eval env) (\v2 ->
        just Value (numV (fromNum v1 - fromNum v2))))
  }
  def mul (e1 : IEval) (e2 : IEval) : IEval = { eval = \env ->
    bind Value Value (e1.eval env) (\v1 ->
      bind Value Value (e2.eval env) (\v2 ->
        just Value (numV (fromNum v1 * fromNum v2))))
  }
  def div (e1 : IEval) (e2 : IEval) : IEval = { eval = \env ->
    bind Value Value (e1.eval env) (\v1 ->
      bind Value Value (e2.eval env) (\v2 ->
        just Value (numV (fromNum v1 / fromNum v2))))
  }
}

-- Pretty pinter algebra
trait ppNatAlg { self =>
  def num (n : Double)                = { print = n.toString }
  def add (e1 : IPrint) (e2 : IPrint) = { print = "(" ++ e1.print ++ " + " ++ e2.print ++ ")"}
  def sub (e1 : IPrint) (e2 : IPrint) = { print = "(" ++ e1.print ++ " - " ++ e2.print ++ ")"}
  def mul (e1 : IPrint) (e2 : IPrint) = { print = "(" ++ e1.print ++ " * " ++ e2.print ++ ")"}
  def div (e1 : IPrint) (e2 : IPrint) = { print = "(" ++ e1.print ++ " / " ++ e2.print ++ ")"}
}

-- Type check algebra
trait tcNatAlg { self =>
  def num (n : Double) : ITC = { tcheck = \_ -> just Type tnum }
  def add (e1 : ITC) (e2 : ITC) : ITC = { tcheck = \env ->
    bind Type Type (e1.tcheck env) (\t1 ->
      caseType Maybe[Type] t1
        (bind Type Type (e2.tcheck env) (\t2 ->
          caseType Maybe[Type] t2
            (just Type tnum)
            (nothing Type)))
        (nothing Type))
  }
  def sub (e1 : ITC) (e2 : ITC) : ITC = { tcheck = \env ->
    bind Type Type (e1.tcheck env) (\t1 ->
      caseType Maybe[Type] t1
        (bind Type Type (e2.tcheck env) (\t2 ->
          caseType Maybe[Type] t2
            (just Type tnum)
            (nothing Type)))
        (nothing Type))
  }
  def mul (e1 : ITC) (e2 : ITC) : ITC = { tcheck = \env ->
    bind Type Type (e1.tcheck env) (\t1 ->
      caseType Maybe[Type] t1
        (bind Type Type (e2.tcheck env) (\t2 ->
          caseType Maybe[Type] t2
            (just Type tnum)
            (nothing Type)))
        (nothing Type))
  }
  def div (e1 : ITC) (e2 : ITC) : ITC = { tcheck = \env ->
    bind Type Type (e1.tcheck env) (\t1 ->
      caseType Maybe[Type] t1
        (bind Type Type (e2.tcheck env) (\t2 ->
          caseType Maybe[Type] t2
            (just Type tnum)
            (nothing Type)))
        (nothing Type))
  }
}


-----------------
--  bool feature
-----------------

type BoolAlg[E] = {
  bool : Bool -> E,
  iff  : E -> E -> E -> E
}


-- Evaluator algebra
trait evalBoolAlg : BoolAlg[IEval] { self =>
  def bool b          = { eval = \_ -> just Value (boolV b) }
  def iff e1 e2 e3 = { eval = \env ->
    bind Value Value (e1.eval env) (\b ->
      if fromBool b
      then e2.eval env
      else e3.eval env)
  }
}

-- Pretty printer algebra
trait ppBoolAlg : BoolAlg[IPrint] { self =>
  def bool b          = { print = b.toString }
  def iff e1 e2 e3    = { print = "(if " ++ e1.print ++ " then " ++ e2.print ++ " else " ++ e3.print ++ ")" }
}

-- Type checker algebra
trait tcBoolAlg : BoolAlg[ITC] { self =>
  def bool b   = { tcheck = \_ -> just Type tbool }
  def iff e1 e2 e3 = { tcheck = \env ->
    bind Type Type (e1.tcheck env) (\t1 ->
      caseType Maybe[Type] t1
        (nothing Type)
        (bind Type Type (e2.tcheck env) (\t2 ->
          (bind Type Type (e3.tcheck env) (\t3 ->
            caseType Maybe[Type] t2
              (caseType Maybe[Type] t3
                (just Type t2)
                (nothing Type))
              (caseType Maybe[Type] t3
                (nothing Type)
                (just Type t2)))))))
  }
}


----------------
--  comp feature
----------------

type CompAlg[E] = {
  eq   : E -> E -> E,
  lt   : E -> E -> E
}

-- Evaluator algebra
trait evalCompAlg : CompAlg[IEval] { self =>
  def eq e1 e2        = { eval = \env ->
    bind Value Value (e1.eval env) (\v1 ->
      bind Value Value (e2.eval env) (\v2 ->
        just Value (boolV (fromNum v1 == fromNum v2))))
  }
  def lt e1 e2        = { eval = \env ->
    bind Value Value (e1.eval env) (\v1 ->
      bind Value Value (e2.eval env) (\v2 ->
        just Value (boolV (fromNum v1 < fromNum v2))))
  }
}


-- Pretty printer algebra
trait ppCompAlg : CompAlg[IPrint] { self =>
  def eq e1 e2        = { print = "(" ++ e1.print ++ " == " ++ e2.print ++ ")" }
  def lt e1 e2        = { print = "(" ++ e1.print ++ " < " ++ e2.print ++ ")" }
}

-- Type checker algebra
trait tcCompAlg : CompAlg[ITC] { self =>
  def eq e1 e2 = { tcheck = \env ->
    bind Type Type (e1.tcheck env) (\t1 ->
      caseType Maybe[Type] t1
        (bind Type Type (e2.tcheck env) (\t2 ->
          caseType Maybe[Type] t2
            (just Type tbool)
            (nothing Type)))
        (nothing Type))
  }
  def lt e1 e2 = { tcheck = \env ->
    bind Type Type (e1.tcheck env) (\t1 ->
      caseType Maybe[Type] t1
        (bind Type Type (e2.tcheck env) (\t2 ->
          caseType Maybe[Type] t2
            (just Type tbool)
            (nothing Type)))
        (nothing Type))
  }
}


----------------
--  logic feature
----------------

type LogicAlg[E] = {
  and : E -> E -> E,
  or : E -> E -> E
}

-- Evaluator algebra
trait evalLogicAlg : LogicAlg[IEval] { self =>
  def and e1 e2        = { eval = \env ->
    bind Value Value (e1.eval env) (\v1 ->
      bind Value Value (e2.eval env) (\v2 ->
        just Value (boolV (fromBool v1 && fromBool v2))))
  }
  def or e1 e2        = { eval = \env ->
    bind Value Value (e1.eval env) (\v1 ->
      bind Value Value (e2.eval env) (\v2 ->
        just Value (boolV (fromBool v1 || fromBool v2))))
  }
}

-- Pretty printer algebra
trait ppLogicAlg : LogicAlg[IPrint] { self =>
  def and e1 e2        = { print = "(" ++ e1.print ++ " && " ++ e2.print ++ ")" }
  def or e1 e2        = { print = "(" ++ e1.print ++ " || " ++ e2.print ++ ")" }
}

-- Type checker algebra
trait tcLogicAlg : LogicAlg[ITC] { self =>
  def and e1 e2 = { tcheck = \env ->
    bind Type Type (e1.tcheck env) (\t1 ->
      caseType Maybe[Type] t1
        (nothing Type)
        (bind Type Type (e2.tcheck env) (\t2 ->
          caseType Maybe[Type] t2
            (nothing Type)
            (just Type tbool))))
  }
  def or e1 e2 = { tcheck = \env ->
    bind Type Type (e1.tcheck env) (\t1 ->
      caseType Maybe[Type] t1
        (nothing Type)
        (bind Type Type (e2.tcheck env) (\t2 ->
          caseType Maybe[Type] t2
            (nothing Type)
            (just Type tbool))))
  }
}



----------------
--  var feature
----------------

type VarAlg[E] = {
  var : String -> E,
  decl : String -> E -> E -> E
}

-- Evaluator algebra
trait evalVarAlg { self =>
  def var (n : String) : IEval = { eval = \env -> lookup Value n env }
  def decl (n : String) (e : IEval) (b : IEval) : IEval = { eval = \env ->
    bind Value Value (e.eval env) (\v ->
      b.eval (insert Value n v env))
  }
}

-- Pretty printer algebra
trait ppVarAlg : VarAlg[IPrint] { self =>
  def var n      = { print = n }
  def decl n e b = { print = "var " ++ n ++ " = " ++ e.print ++ "; " ++ b.print }
}

-- Type checker algebra
trait tcVarAlg : VarAlg[ITC] { self =>
  def var n      = { tcheck = \env -> lookup Type n env }
  def decl n e b = { tcheck = \env ->
    bind Type Type (e.tcheck env) (\t ->
      b.tcheck (insert Type n t env))
  }
}



-----------------------------
--  function feature
-----------------------------

type TFunAlg[E] = {
  call : String -> E -> E
}


-- Pretty printer algebra
trait ppFunAlg : TFunAlg[IPrint] { self =>
  def call f arg = { print = f ++ "(" ++ arg.print ++ ")" }
}

-- Type checker algebra
type FTEnv = EnvF[Pair[Type, Type]]

trait tcFunAlg(ftenv : FTEnv) : TFunAlg[ITC] { self =>
  def call f arg = { tcheck = \env ->
    bind Pair[Type,Type] Type (lookup Pair[Type,Type] f ftenv) (\p ->
      casePair Type Type Maybe[Type] p (\a b ->
        bind Type Type (arg.tcheck env) (\a' ->
          if eqTypes a a' then just Type b else nothing Type)))
  }
}


---------------------------------------
-- Languages
---------------------------------------

------------
-- simplenat
------------

-- AST
type SNat = { accept : forall E. NatAlg[E] -> E }

-- Evaluator
def evalNat (e : SNat) : IEval = e.accept IEval (new[NatAlg[IEval]] evalNatAlg)

-- Pretty printer
def ppNat (e : SNat) : IPrint = e.accept IPrint (new[NatAlg[IPrint]] ppNatAlg)


------------
-- simplebool
------------

-- AST
type SBool = { accept : forall E. BoolAlg[E] -> E }

-- Evaluator
def evalBool (e : SBool) : IEval = e.accept IEval (new[BoolAlg[IEval]] evalBoolAlg)

-- Pretty printer
def ppBool (e : SBool) : IPrint = e.accept IPrint (new[BoolAlg[IPrint]] ppBoolAlg)


------------
-- natbool
------------

-- AST
type NatBoolAlg[E] = NatAlg[E] & BoolAlg[E]
type NatBool = { accept : forall E. NatBoolAlg[E] -> E }

-- Evaluator
def evalNatBool (e : NatBool) : IEval = e.accept IEval (new[NatBoolAlg[IEval]] evalNatAlg & evalBoolAlg)

-- Pretty printer
def ppNatBool (e : NatBool) : IPrint = e.accept IPrint (new[NatBoolAlg[IPrint]] ppNatAlg & ppBoolAlg)

-- Type checker
def tcNatBool (e : NatBool) = e.accept ITC (new[NatBoolAlg[ITC]] tcNatAlg & tcBoolAlg)



------------
-- varbool
------------

type VarBoolAlg[E] = BoolAlg[E] & VarAlg[E]
type VarBool = { accept : forall E. VarBoolAlg[E] -> E }


-- Evaluator
def evalVarBool (e : VarBool) : IEval = e.accept IEval (new[VarBoolAlg[IEval]] evalBoolAlg & evalVarAlg)

-- Pretty printer
def ppVarBool (e : VarBool) : IPrint = e.accept IPrint (new[VarBoolAlg[IPrint]] ppBoolAlg & ppVarAlg)


------------
-- varnat
------------

type VarNatAlg[E] = NatAlg[E] & VarAlg[E]
type VarNat = { accept : forall E. VarNatAlg[E] -> E }


-- Evaluator
def evalVarNat (e : VarNat) : IEval = e.accept IEval (new[VarNatAlg[IEval]] evalNatAlg & evalVarAlg)

-- Pretty printer
def ppVarNat (e : VarNat) : IPrint = e.accept IPrint (new[VarNatAlg[IPrint]] ppNatAlg & ppVarAlg)



-------------
-- simplelogic
-------------

type BoolLogicAlg[E] = BoolAlg[E] & LogicAlg[E]
type BoolLogic = { accept : forall E. BoolLogicAlg[E] -> E }


-- Evaluator
def evalBoolLogic (e : BoolLogic) : IEval = e.accept IEval (new[BoolLogicAlg[IEval]] evalBoolAlg & evalLogicAlg)

-- Pretty printer
def ppBoolLogic (e : BoolLogic) : IPrint = e.accept IPrint (new[BoolLogicAlg[IPrint]] ppBoolAlg & ppLogicAlg)


----------------
-- arith
----------------

-- BEGIN_ARITH
type ArithAlg[E] = NatBoolAlg[E] & CompAlg[E]
type Arith = { accept : forall E. ArithAlg[E] -> E }
-- Evaluator
def evalArith (e : Arith) : IEval =
  e.accept IEval (new[ArithAlg[IEval]] evalNatAlg & evalBoolAlg & evalCompAlg)
-- Pretty printer
def ppArith (e : Arith) : IPrint =
  e.accept IPrint (new[ArithAlg[IPrint]] ppNatAlg & ppBoolAlg & ppCompAlg)
-- Type checker
def tcArith (e : Arith) =
  e.accept ITC (new[ArithAlg[ITC]] tcNatAlg & tcBoolAlg & tcCompAlg)
-- END_ARITH


----------------
-- arithlogic
----------------

type ArithLogicAlg[E] = ArithAlg[E] & LogicAlg[E]
type ArithLogic = { accept : forall E. ArithLogicAlg[E] -> E }


-- Evaluator
def evalArithLogic (e : ArithLogic) : IEval =
  e.accept IEval (new[ArithLogicAlg[IEval]] evalNatAlg & evalBoolAlg & evalCompAlg & evalLogicAlg)

-- Pretty printer
def ppArithLogic (e : ArithLogic) : IPrint =
  e.accept IPrint (new[ArithLogicAlg[IPrint]] ppNatAlg & ppBoolAlg & ppCompAlg & ppLogicAlg)

-- Type checker
def tcArithLogic (e : ArithLogic) =
  e.accept ITC (new[ArithLogicAlg[ITC]] tcNatAlg & tcBoolAlg & tcCompAlg & tcLogicAlg)


----------------
-- varlogic
----------------

type VarLogicAlg[E] = BoolLogicAlg[E] & VarAlg[E]
type VarLogic = { accept : forall E. VarLogicAlg[E] -> E }


-- Evaluator
def evalVarLogic (e : VarLogic) : IEval =
  e.accept IEval (new[VarLogicAlg[IEval]] evalBoolAlg & evalLogicAlg & evalVarAlg)

-- Pretty printer
def ppVarLogic (e : VarLogic) : IPrint =
  e.accept IPrint (new[VarLogicAlg[IPrint]] ppBoolAlg & ppLogicAlg & ppVarAlg)


----------------
-- vararith
----------------

type VarArithAlg[E] = ArithAlg[E] & VarAlg[E]
type VarArith = { accept : forall E. VarArithAlg[E] -> E }


-- Evaluator
def evalVarArith (e : VarArith) : IEval =
  e.accept IEval (new[VarArithAlg[IEval]] evalNatAlg & evalBoolAlg & evalCompAlg & evalVarAlg)

-- Pretty printer
def ppVarArith (e : VarArith) : IPrint =
  e.accept IPrint (new[VarArithAlg[IPrint]] ppNatAlg & ppBoolAlg & ppCompAlg & ppVarAlg)

-- Type checker
def tcVarArith (e : VarArith) =
  e.accept ITC (new[VarArithAlg[ITC]] tcNatAlg & tcBoolAlg & tcCompAlg & tcVarAlg)



-----------------------
-- vararithlogic
-----------------------

type VarArithLogicAlg[E] = ArithLogicAlg[E] & VarAlg[E]
type VarArithLogic = { accept : forall E. VarArithLogicAlg[E] -> E }

-- Evaluator
def evalVarArithLogic (e : VarArithLogic) : IEval =
  e.accept IEval (new[VarArithLogicAlg[IEval]] evalNatAlg & evalBoolAlg & evalCompAlg & evalLogicAlg & evalVarAlg)

-- Pretty printer
def ppVarArithLogic (e : VarArithLogic) : IPrint =
  e.accept IPrint (new[VarArithLogicAlg[IPrint]] ppNatAlg & ppBoolAlg & ppCompAlg & ppLogicAlg & ppVarAlg)

-- Type checker
def tcVarArithLogic (e : VarArithLogic) =
  e.accept ITC (new[VarArithLogicAlg[ITC]] tcNatAlg & tcBoolAlg & tcCompAlg & tcLogicAlg & tcVarAlg)



------------
-- Mini-JS
------------

-- AST
type MiniJSAlg[E] = VarArithLogicAlg[E] & TFunAlg[E]
type MiniJS = { accept : forall E. MiniJSAlg[E] -> E }

type FunAlg[E] = {
  fun : String -> Type -> Type -> MiniJS -> E
}

type Function = {
  accept : forall E. FunAlg[E] -> E
}

def caseFn E (x : Function) (f : String -> Type -> Type -> MiniJS -> E) : E = x.accept E { fun = f }

type FEnv = EnvF[Function]

type PgmAlg[E] = {
  pgm : FEnv -> MiniJS -> E
}

type Program = {
  accept : forall E. PgmAlg[E] -> E
}

def casePgm E (p : Program) (f: FEnv -> MiniJS -> E) : E = p.accept E { pgm = f }


-- Evaluator algbebra
trait evalFunAlg(fenv : FEnv) inherits evalNatAlg & evalBoolAlg &  evalCompAlg & evalLogicAlg & evalVarAlg
  : MiniJSAlg[IEval] { self : MiniJSAlg[IEval] =>
  def call f arg = { eval = \env ->
    bind Function Value (lookup Function f fenv) (\fn ->
      caseFn Maybe[Value] fn
        (\param _ _ body ->
          bind Value Value (arg.eval env)
            (\v -> (body.accept IEval self).eval (insert Value param v env))))
  }
}


-- Evaluator
def evalMiniJS (e : MiniJS) (fenv : FEnv) : IEval =
  e.accept IEval (new[MiniJSAlg[IEval]] evalFunAlg(fenv))

-- Pretty printer
def ppMiniJS (e : MiniJS) : IPrint =
  e.accept IPrint (new[MiniJSAlg[IPrint]] ppBoolAlg & ppNatAlg & ppCompAlg & ppVarAlg & ppLogicAlg & ppFunAlg)

-- Type checker
def tcMiniJS (e : MiniJS) (ftenv : FTEnv) : ITC =
  e.accept ITC (new[MiniJSAlg[ITC]] tcBoolAlg & tcNatAlg & tcCompAlg & tcVarAlg & tcLogicAlg & tcFunAlg(ftenv))



-----------------------
-- Put all together
-----------------------

def checkFEnv (fenv : FEnv) : FTEnv = \n ->
  bind Function Pair[Type, Type] (lookup Function n fenv) (\f ->
    caseFn Maybe[Pair[Type, Type]] f (\arg typ ret body ->
    (just Pair[Type,Type] (mkPair Type Type typ ret))))

-- Combined Algebra, supporting evaluator, pretty printer and type checker
type SuperAlg = IEval & IPrint & ITC
def combineAlg (fenv : FEnv) : MiniJSAlg[SuperAlg] =
  let ftenv : FTEnv = checkFEnv fenv in
  let op1 : MiniJSAlg[IEval]  = new[MiniJSAlg[IEval]] evalFunAlg(fenv) in
  let op2 : MiniJSAlg[IPrint] = new[MiniJSAlg[IPrint]] ppBoolAlg & ppNatAlg & ppCompAlg & ppVarAlg & ppLogicAlg & ppFunAlg in
  let op3 : MiniJSAlg[ITC]    = new[MiniJSAlg[ITC]] tcBoolAlg & tcNatAlg & tcCompAlg & tcVarAlg & tcLogicAlg & tcFunAlg(ftenv) in
  op1 ,, op2 ,, op3


def add1Body : MiniJS = {
  accept = /\E. \f -> f.add (f.var "x") (f.num 1)
}

def add1 : Function = {
  accept = /\E. \f -> f.fun "x" tnum tnum add1Body
}

def fenv : FEnv = insert Function "add1" add1 (empty Function)

-- add1(var x = 3; var y = 4; if x < y then x + 2 else y + 3)
def test : Program = {accept = /\E. \p -> p.pgm fenv { accept = /\F. \f ->
  f.call "add1"
    (f.decl "x" (f.num 3)
      (f.decl "y" (f.num 4)
        (f.iff (f.lt (f.var "x") (f.var "y"))
          (f.add (f.var "x") (f.num 2))
          (f.add (f.var "y") (f.num 3)))))}}

-- A function that ensures "well-typed programs cannot go wrong"
def evalPgm (p : Program) : String =
  casePgm String p (\fenv m ->
    let ftenv : FTEnv = checkFEnv fenv in
    if isJust Type ((tcMiniJS m ftenv).tcheck (empty Type))
    then (ppMiniJS m).print ++ " = " ++
         (fromNum (fromJust Value ((evalMiniJS m fenv).eval (empty Value)))).toString
    else "Type error!")

-- BEGIN_TEST_TEST
main = evalPgm test
-- Output: "add1(var x = 3.0; var y = 4.0; (if (x < y) then (x + 2.0) else (y + 3.0))) = 6.0"
-- END_TEST_TEST
