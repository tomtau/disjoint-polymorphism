--> "7.0 + 2.0 - 4.0 = 5.0"

type IEval = {eval : Double}

type ExpAlg[E] = {
  lit : Double -> E,
  add : E -> E -> E
}

def evalAlg : ExpAlg[IEval] = {
  lit = \x -> {eval = x},
  add = \x y -> {eval = x.eval + y.eval}
}

type SubExpAlg[E] = ExpAlg[E] & { sub : E -> E -> E }

def subEvalAlg : SubExpAlg[IEval] = evalAlg ,, {
  sub = \x y -> { eval = x.eval - y.eval }
}

type IPrint = { print : String }

def printAlg : SubExpAlg[IPrint] = {
  lit = \x -> { print = x.toString },
  add = \x y -> { print = x.print ++ " + " ++ y.print },
  sub = \x y -> { print = x.print ++ " - " ++ y.print }
}


{-

We don't have to manually write the combination as follows, subtyping handles that

def combine A [B * A] (f : ExpAlg[A]) (g : ExpAlg[B]) : ExpAlg[A & B] = {
  lit = \x -> f.lit x ,, g.lit x,
  add = \x y -> f.add x y ,, g.add x y
}

-}

def combine A [B * A] (f : SubExpAlg[A]) (g : SubExpAlg[B]) = f ,, g

def e1 E (f : ExpAlg[E]) = f.add (f.lit 7) (f.lit 2)

def e2 E (f : SubExpAlg[E]) = f.sub (e1 E f) (f.lit 4)

def evalPrintAlg : SubExpAlg[IEval & IPrint] = combine IEval IPrint subEvalAlg printAlg

val o = e2 (IEval & IPrint) evalPrintAlg

main = o.print ++ " = " ++ o.eval.toString
