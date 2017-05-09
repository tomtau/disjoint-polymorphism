--> true

type GCircuitAlg[In, Out] = {
  identity : Double -> Out,
  fan : Double -> Out,
  beside : In -> In -> Out,
  above : In -> In -> Out
}

type CircuitAlg[E] = GCircuitAlg[E, E]

type Circuit = { accept : forall E. CircuitAlg[E] -> E }

def e1 : Circuit = { accept = /\E . \f ->
  f.above (f.beside (f.fan 2) (f.fan 2))
          (f.beside (f.beside (f.identity 1) (f.fan 2)) (f.identity 1))
}

-- Width interpretation
type Width = { width : Double }

trait widthCircuit : CircuitAlg[Width] { self =>
  def identity n = { width = n }
  def fan n = { width = n }
  def beside c1 c2 = { width = c1.width + c2.width }
  def above c1 c2 = { width = c1.width }
}

def width (c : Circuit) : Double = (c.accept Width (new[CircuitAlg[Width]] widthCircuit)).width


-- Depth interpretation
type Depth = { depth : Double }

def max (x : Double) (y : Double) = if x > y then x else y

trait depthCircuit : CircuitAlg[Depth] { self =>
  def identity n = { depth = 0 }
  def fan n = { depth = 1 }
  def beside c1 c2 = { depth = max c1.depth c2.depth }
  def above c1 c2 = { depth = c1.depth + c2.depth }
}

def depth (c : Circuit) : Double = (c.accept Depth (new[CircuitAlg[Depth]] depthCircuit)).depth

-- Well-sized interpretation

type WellSized = { wellSized : Bool }

trait sizedCircuit : GCircuitAlg[Width & WellSized, WellSized] { self =>
  def identity n = { wellSized = true }
  def fan n = { wellSized = true }
  def beside c1 c2 = { wellSized = c1.wellSized && c2.wellSized }
  def above c1 c2 = { wellSized = c1.wellSized && c2.wellSized && c1.width == c2.width }
}

trait merge A [B * A] (a : Trait[CircuitAlg[A]], b : Trait[GCircuitAlg[A & B, B]]) : CircuitAlg[A & B] { self =>
  def identity n = (new[CircuitAlg[A]] a).identity n ,, (new[GCircuitAlg[A & B, B]] b).identity n
  def fan n = (new[CircuitAlg[A]] a).fan n ,, (new[GCircuitAlg[A & B, B]] b).fan n
  def beside c1 c2 = (new[CircuitAlg[A]] a).beside c1 c2 ,, (new[GCircuitAlg[A & B, B]] b).beside c1 c2
  def above c1 c2 = (new[CircuitAlg[A]] a).above c1 c2 ,, (new[GCircuitAlg[A & B, B]] b).above c1 c2
}

def newAlg = merge Width WellSized widthCircuit sizedCircuit

def o = new[CircuitAlg[Width & WellSized]] newAlg

main = (e1.accept (Width & WellSized) o).wellSized
