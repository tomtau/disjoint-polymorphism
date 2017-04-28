--> "2.0 + 3.0 and 5 + 4 = 9.0"

type ExpAlg[E] = {
  lit : Double -> E,
  add : E -> E -> E
}

type Exp = { accept : forall E . ExpAlg[E] -> E }

type IEval = { eval : Double }
type IPrint = { print : String }


trait evalAlg : ExpAlg[IEval] { self =>
  def lit x   = { eval = x }
  def add x y = { eval = x.eval + y.eval }
}


def e1 : Exp = { accept = /\E . \f -> f.add (f.lit 2) (f.lit 3) }


-- Family self reference
trait printAlg3 : ExpAlg[IPrint] { fself : ExpAlg[IEval & IPrint] =>
  def lit x  = { print = x.toString }
  def add e1 e2 = {print =
    let plus54 : IEval = fself.add (fself.lit 5) (fself.lit 4)
    in e1.print ++ " + " ++ e2.print ++ " and " ++ "5 + 4 = " ++ plus54.eval.toString
  }
}

trait evalAlg2 : ExpAlg[IEval] { self =>
  def lit x = { eval = x + 1 }
  def add x y = { eval = x.eval + y.eval }
}

def o = new[ExpAlg[IEval & IPrint]] evalAlg & printAlg3

main = (e1.accept (IEval & IPrint) o).print
