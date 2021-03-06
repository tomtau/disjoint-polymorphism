--> "((5.0 - (2.0 + 3.0)) + 3.0) = 3.0"

type ExpAlg[E] = {
  lit : Double -> E,
  add : E -> E -> E
}

type Exp = { accept : forall E . ExpAlg[E] -> E }

type IEval = { eval : Double }

trait evalAlg : ExpAlg[IEval] { self =>
  def lit x   = { eval = x }
  def add x y = { eval = x.eval + y.eval }
}


type SubExpAlg[E] = ExpAlg[E] & { sub : E -> E -> E }
trait subEvalAlg inherits evalAlg : SubExpAlg[IEval]  { self =>
  def sub x y = { eval = x.eval - y.eval }
}
type ExtExp = { accept: forall E. SubExpAlg[E] -> E }



type IPrint = { print : String }


trait printAlg : SubExpAlg[IPrint] { self =>
  def lit x   = { print = x.toString }
  def add x y = { print = "(" ++ x.print ++ " + " ++ y.print ++ ")" }
  def sub x y = { print = "(" ++ x.print ++ " - " ++ y.print ++ ")" }
}



def lit (n : Double) : Exp = {
  accept = /\E . \f -> f.lit n
}
def add (e1 : Exp) (e2 : Exp) : Exp = {
  accept = /\E . \f -> f.add (e1.accept E f) (e2.accept E f)
}
def sub (e1 : ExtExp) (e2 : ExtExp) : ExtExp = {
  accept = /\E . \f -> f.sub (e1.accept E f) (e2.accept E f)
}


trait combine1 A [B * A] (f : Trait[SubExpAlg[A]] , g : Trait[SubExpAlg[B]]) { self =>
  def lit (x : Double)   = (new[SubExpAlg[A]] f).lit x   ,, (new[SubExpAlg[B]] g).lit x
  def add (x : A & B) (y : A & B) = (new[SubExpAlg[A]] f).add x y ,, (new[SubExpAlg[B]] g).add x y
  def sub (x : A & B) (y : A & B) = (new[SubExpAlg[A]] f).sub x y ,, (new[SubExpAlg[B]] g).sub x y
}



-- BEGIN_COMBINE_DEF
trait combine A [B * A] (f : Trait[SubExpAlg[A]], g : Trait[SubExpAlg[B]]) inherits f & g
-- END_COMBINE_DEF

def e1 : Exp = {accept = /\E . \f -> f.add (f.lit 2) (f.lit 3)}
def e2 : ExtExp = { accept = /\E. \f -> f.sub (f.lit 5) (e1.accept E f) }
def e3 : ExtExp = { accept = /\E. \f -> f.add (e2.accept E f) (f.lit 3) }


-- BEGIN_COMBINE1_TEST
def newAlg = combine IEval IPrint subEvalAlg printAlg
def o = e3.accept (IEval & IPrint) (new[SubExpAlg[IEval & IPrint]] newAlg)
main = o.print ++ " = " ++ o.eval.toString
-- Output: "((5.0 - (2.0 + 3.0)) + 3.0) = 3.0"
-- END_COMBINE1_TEST
