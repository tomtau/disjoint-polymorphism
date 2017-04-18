--> "Round button has area: 28.26"

-- BEGIN_POINT_DEF
type Point = { x : Double, y : Double }
trait point(x : Double, y: Double) { self =>
  def x = x
  def y = y }
-- END_POINT_DEF

-- BEGIN_POINT_TEST
def pointTest  = new[Point] point(3, 4)
-- END_POINT_TEST

def abs (x : Double) = if x < 0 then (0 - x) else x

def square (x : Double) = x * x


-- BEGIN_CIRCLE_DEF
type Circle = Point & {radius : Double}
trait circle(center : Point, radius : Double) inherits point(center.x, center.y) { self =>
  def radius = radius }
-- END_CIRCLE_DEF

-- BEGIN_CIRCLE_TEST
def circleTest = new[Circle] circle(pointTest, 3)
-- END_CIRCLE_TEST

-- BEGIN_CIRCLE_FNS
type CircleFns = { area : T -> Double, grow : T -> Double, shrink : T -> Double }
trait circleFns { self : Circle =>
  def area()   = self.radius * self.radius * 3.14
  def grow()   = self.radius + 1
  def shrink() = self.radius - 1 }
-- END_CIRCLE_FNS

-- BEGIN_CIRCLE_FULL
def circleWithFns = new[Circle & CircleFns] circle(pointTest, 3) & circleFns
-- END_CIRCLE_FULL


-- BEGIN_BUTTON_DEF
type Button = { label : String }
trait button(label : String) { self => def label = label }
-- END_BUTTON_DEF

-- BEGIN_BUTTON_FNS
type ButtonFns = { hover : Bool -> String, press : Bool -> String }
trait buttonFns { self : Button =>
  def hover (b : Bool) = if b then "hovering..." else "no hovering"
  def press (b : Bool) = if b then "pressing..." else "no pressing" }
-- END_BUTTON_FNS

-- BEGIN_ROUNDBUTTON_DEF
type RoundButton = Circle & Button
trait roundButton(radius : Double, center: Point, label : String)
  inherits circle(center, radius) & button(label)
-- END_ROUNDBUTTON_DEF


-- BEGIN_ASOVAL_DEF
trait asOval(shortRadius : Double, longRadius : Double) { self =>
  def radius = shortRadius
  def longRadius = longRadius }
-- END_ASOVAL_DEF

{-
-- BEGIN_CONFLICT_DEF
-- Invalid SEDEL code:
trait oval(shortRadius : Double, longRadius : Double, center: Point)
  inherits circle(center, shortRadius) & asOval(shortRadius, longRadius)
-- END_CONFLICT_DEF
-}


-- BEGIN_CONFLICT_RESOLVE
trait oval(shortRadius : Double, longRadius : Double, center: Point)
  inherits circle(center, shortRadius) \ { radius : Double } & asOval(shortRadius, longRadius)
-- END_CONFLICT_RESOLVE

-- BEGIN_NORM_DEF
type Norm = { norm : Double -> Double -> Double }
trait euclideanNorm { self : Point =>
  def norm (x : Double) (y : Double) = (square(self.x - x) + square(self.y - y)).sqrt }
trait manhattanNorm { self : Point =>
  def norm (x : Double) (y : Double) = abs((self.x - x)) + abs((self.y - y)) }
-- END_NORM_DEF

-- BEGIN_CIRCLE_FNS2
type CircleFns2 = CircleFns & { inCircle : Double -> Double -> Bool }
trait circleFns2 inherits circleFns { self : Circle & Norm =>
  def inCircle (x : Double) (y : Double) = self.norm x y < self.radius }
-- END_CIRCLE_FNS2

-- BEGIN_POINT_FUNC
def roundButtonFac (radius : Double) (center : Point) (norm : Trait[Point, Norm]) =
  new[RoundButton & CircleFns2 & ButtonFns & Norm]
    roundButton(radius, center, "Round button") & circleFns2 & buttonFns & norm
-- END_POINT_FUNC

-- BEGIN_ROUNDBUTTON_TEST2
def roundButtonTest2 = roundButtonFac 3 pointTest euclideanNorm
-- END_ROUNDBUTTON_TEST2

def test = roundButtonTest2.inCircle 3 4


-- BEGIN_ROUNDBUTTON_TEST
def roundButtonTest = new[RoundButton & CircleFns & ButtonFns]
  roundButton(3, pointTest, "Round button") & circleFns & buttonFns
main = roundButtonTest.label ++ " has area: " ++ (roundButtonTest.area()).toString
-- Output: "Round button has area: 28.26"
-- END_ROUNDBUTTON_TEST
