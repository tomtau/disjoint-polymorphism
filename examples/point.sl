--> 3.0


type Point = { x : Double, y : Double };
trait point(x : Double, y: Double) { self : Point =>
  x = x;
  y = y
};

type Point3D = Point & { z : Double };

-- BEGIN_DESUGAR1
trait point3D(x: Double, y : Double) inherits point(x,y) { self : Point3D => z = self.x };
-- END_DESUGAR1

test = new[Point3D] point3D(3,4);

main = test.x
