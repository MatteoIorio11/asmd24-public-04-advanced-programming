package scala.u04.point
import org.scalacheck.*
import org.scalacheck.Prop.*
import u04.point.Point2D
import java.lang.Math.*
import scala.math.Pi

/**
 * Write a comprehensive ScalaCheck test suite that verifies mathematical properties such as: Distance symmetry (i.e., a.distanceTo(b) == b.distanceTo(a)), Triangle inequality, Rotation
 * invariants
 */
object Point2DGenerators:
  val genDouble: Gen[Double] = Gen.choose(-1e6, 1e6)

  val genPoint: Gen[(Double, Double)] = for {
    x <- genDouble
    y <- genDouble
  } yield (x, y)

  val genPoint2D: Gen[Point2D] =
    genPoint.map { case (x, y) => new Point2D(x, y) }

  val genAngle: Gen[Double] = Gen.choose(-Pi, Pi)

/**
 * Write a comprehensive ScalaCheck test suite that verifies mathematical properties such as: Distance symmetry (i.e., a.distanceTo(b) == b.distanceTo(a)), Triangle inequality, Rotation
 * invariants
 */
object Point2DSpec extends Properties("Point2D"):
  import Point2DGenerators._

  /** Distance Law
   * The distance between two points should be the same.
   * distance(a, b) == distance(b, a)
   * distance(c, b) == distance(b, c)
   */
  property("distance symmetry") = Prop.forAll(genPoint2D, genPoint2D) { (a, b) =>
    val d1 = a.distanceTo(b)
    val d2 = b.distanceTo(a)
    (d1 - d2).abs < 1e-9
  }

  /**
   * <<WIKIPEDIA>>
   * In mathematics, the triangle inequality states that for any triangle, the sum of the lengths of any two sides must be greater than or equal to the length of the remaining side
   */
  property("triangle inequality") = Prop.forAll(genPoint2D, genPoint2D, genPoint2D) { (a, b, c) =>
    val ab = a.distanceTo(b)
    val bc = b.distanceTo(c)
    val ac = a.distanceTo(c)
    ac <= ab + bc + 1e-9 // allow for floating-point slack
  }


  /** Rotation Law
   * If we rotate a point, the distance to the origin should be always the same.
   * distance(origin, a) == distance(origin, rotate(a, angle))
   */
  property("rotation preserves distance to origin") = Prop.forAll(genPoint2D, genAngle) { (p, angle) =>
    val origin = new Point2D(0, 0)
    val rotated = p.rotate(angle)
    val d1 = p.distanceTo(origin)
    val d2 = rotated.distanceTo(origin)
    (d1 - d2).abs < 1e-9
  }

  /** Full Rotation Law
   * If we rotate the point of 2pi, the point should be on the same starting point.
   * distance(a, rotate(a, 2pi)) = 0.0
   */
  property("rotate full circle yields same point (approximately)") = Prop.forAll(genPoint2D) { p =>
    val fullCircle = 2 * Pi
    val rotated = p.rotate(fullCircle)
    p.distanceTo(rotated) < 1e-9
  }

