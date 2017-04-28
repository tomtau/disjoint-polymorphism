--> 3.0


type Point = { x : Double, y : Double }
trait point(x : Double, y: Double) { self : Point =>
  def x = x
  def y = y
}

type Point3D = Point & { z : Double }

-- BEGIN_DESUGAR1
trait point3D(x: Double, y : Double) inherits point(x,y) { self : Point3D => def z = self.x }
-- END_DESUGAR1

def point4D' (x : Double) (y : Double) (self : Point3D) = point x y self ,, { z = self.x }


def test = new[Point3D] point3D(3,4)

def test' = let self : Point3D = point3D 3 4 self in self

main = test.x
