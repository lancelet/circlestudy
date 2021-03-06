package pwa

import c3d._
import scala.annotation.tailrec
import scala.collection.immutable._
import math.{atan2, cos, sin, sqrt}
import org.apache.commons.math3.analysis.MultivariateFunction
import org.apache.commons.math3.optim.InitialGuess
import org.apache.commons.math3.optim.MaxEval
import org.apache.commons.math3.optim.PointValuePair
import org.apache.commons.math3.optim.SimpleBounds
import org.apache.commons.math3.optim.nonlinear.scalar.GoalType
import org.apache.commons.math3.optim.nonlinear.scalar.ObjectiveFunction
import org.apache.commons.math3.optim.nonlinear.scalar.noderiv.BOBYQAOptimizer
import c3d.util.transform.RotationMatrix

object Geom {

  final case class Vec2D(x: Float, y: Float) {
    def -(v: Vec2D): Vec2D = Vec2D(x - v.x, y - v.y)
    def *(s: Float): Vec2D = Vec2D(x * s, y * s)
    def dot(v: Vec2D): Float = x * v.x + y * v.y
    def angle: Float = atan2(y, x).toFloat
    def mag: Float = sqrt(x * x + y * y).toFloat
    def asVec3D: Vec3D = Vec3D(x, y, 0)
  }
  object Vec2D {
    def fromVec3Dxy(v3: Vec3D): Vec2D = Vec2D(v3.x, v3.y)
    def fromPolar(r: Float, theta: Float): Vec2D = Vec2D((r * cos(theta)).toFloat, (r * sin(theta)).toFloat)
  }

  trait XForm2D {
    def apply(v: Vec2D): Vec2D
  }

  final case class Rot2D(angleRad: Double) extends XForm2D {
    private val c = cos(angleRad)
    private val s = sin(angleRad)
    def apply(v: Vec2D): Vec2D = Vec2D((v.x * c - v.y * s).toFloat, (v.x * s + v.y * c).toFloat)
    def asRotationMatrix: RotationMatrix = {
      val cf = c.toFloat
      val sf = s.toFloat
      RotationMatrix(cf, -sf, 0, sf, cf, 0, 0, 0, 1)
    }
  }

  final case class Circle(origin: Vec2D, radius: Float)
  
  final case class Line(origin: Vec2D, vec: Vec2D)
  
  def distanceBetweenPoints(p1: Vec2D, p2: Vec2D): Float = {
    val dx: Float = p2.x - p1.x
    val dy: Float = p2.y - p1.y
    sqrt(dx * dx + dy * dy).toFloat
  }  
  
  def lineIntersection(l1: Line, l2: Line): Vec2D = {
    val det: Float = l1.vec.x * l2.vec.y - l2.vec.x * l1.vec.y
    val c1: Float = -l1.vec.y / det
    val c2: Float =  l1.vec.x / det
    val dox: Float = l1.origin.x - l2.origin.x
    val doy: Float = l1.origin.y - l2.origin.y
    val f: Float = c1 * dox + c2 * doy
    val x: Float = l2.origin.x + f * l2.vec.x
    val y: Float = l2.origin.y + f * l2.vec.y
    Vec2D(x, y)
  }
  
  def circleThrough3Points(p1: Vec2D, p2: Vec2D, p3: Vec2D): Circle = {
    val bisector1: Line = perpendicularBisector(p1, p2)
    val bisector2: Line = perpendicularBisector(p2, p3)
    val origin: Vec2D = lineIntersection(bisector1, bisector2)
    val radius: Float = distanceBetweenPoints(origin, p2)
    Circle(origin, radius)
  }
  
  def perpendicularBisector(p1: Vec2D, p2: Vec2D): Line = {
    val origin_x: Float = (p1.x + p2.x) / 2.0f
    val origin_y: Float = (p1.y + p2.y) / 2.0f
    val dx: Float = p2.x - origin_x
    val dy: Float = p2.y - origin_y
    val vecx: Float = -dy
    val vecy: Float = +dx
    val mag: Float = sqrt(vecx * vecx + vecy * vecy).toFloat
    Line(Vec2D(origin_x, origin_y), Vec2D(vecx / mag, vecy / mag))
  }  
  
  def lsqCircle(xx: IndexedSeq[Vec2D]): Circle = {
    
    // find initial guess from 3 points
    val pt3circle = circleThrough3Points(xx(0), xx(xx.length / 2), xx.last)
    val initialGuess = new InitialGuess(
      Array(
        pt3circle.origin.x.toDouble,
        pt3circle.origin.y.toDouble,
        pt3circle.radius.toDouble
      )
    ) // x,y,r
    
    // general multi-variate optimisation
    val fitFunction: MultivariateFunction = new MultivariateFunction {
      def value(xyr: Array[Double]): Double = {
        @tailrec def accum(err: Double, index: Int): Double = {
          if (index == xx.length) {
            err
          } else {
            val v: Vec2D = xx(index)
            val dx = v.x - xyr(0)            // x coordinate relative to current origin
            val dy = v.y - xyr(1)            // y coordinate relative to current origin
            val dr = sqrt(dx * dx + dy * dy)  // current radius of (x,y) point
            val delta = dr - xyr(2)           // error in the radius of the (x,y) point
            accum(err + (delta * delta), index + 1)
          }
        }
        accum(0.0, 0)
      }
    }
    val objectiveFn: ObjectiveFunction = new ObjectiveFunction(fitFunction)
    val optimizer = new BOBYQAOptimizer(5)
    val maxEval = new MaxEval(1000)
    val bounds = SimpleBounds.unbounded(3)
    val pvp: PointValuePair = optimizer.optimize(objectiveFn, initialGuess, maxEval, GoalType.MINIMIZE, bounds)
    
    // extract values
    val a: Array[Double] = pvp.getPoint()
    val x = a(0).toFloat
    val y = a(1).toFloat
    val r = a(2).toFloat
    
    Circle(Vec2D(x, y), r)
    
  }
  
  // Point that memoized calculations on another point
  final class MemoizedPoint(sourcePoint: Point) extends Point {
    val name: String = sourcePoint.name
    val description: String = sourcePoint.description
    val rate: Float = sourcePoint.rate
    def asMarker: Marker = throw new NotImplementedError("asMarker not implemented for MemoizedPoint")
    val length: Int = sourcePoint.length
    private [this] val vecArray: Array[Option[Vec3D]] = sourcePoint.toArray
    def apply(index: Int): Option[Vec3D] = vecArray(index)
  }

  /**
   * Computes the average coordinate of some point over a given range.
   *
   * Samples at which the point is undefined are ignored.
   *
   * @param p point
   * @param start start of the range (inclusive)
   * @param end end of the range (inclusive)
   * @return average coordinates of the point
   */
  def averagePt(p: Point, start: Int, end: Int): Vec3D = {
    var x, y, z: Float = 0.0f
    var n: Int = 0

    var i: Int = start
    while (i <= end) {
      val vOpt: Option[Vec3D] = p(i)
      if (vOpt.isDefined) {
        val v: Vec3D = vOpt.get
        x += v.x; y += v.y; z += v.z
        n += 1
      }
      i += 1
    }

    Vec3D(x / n, y / n, z / n)
  }

  /**
   * Computes the average coordinates of a point over its entire range.
   *
   * Samples at which the point is undefined are ignored.
   *
   * @param p point
   * @return average coordinates
   */
  def averagePt(p: Point): Vec3D = averagePt(p, 0, p.length - 1)

  /**
   * Triangle class that can test whether points are within its periphery.
   *
   * @param a corner of the triangle
   * @param b corner of the triangle
   * @param c corner of the triangle
   */
  case class Triangle(a: Vec2D, b: Vec2D, c: Vec2D) {
    private val det: Float = (a.x - c.x) * (b.y - c.y) - (b.x - c.x) * (a.y - c.y)
    def within(p: Vec2D): Boolean = {
      val alpha = ((b.y - c.y) * (p.x - c.x) + (c.x - b.x) * (p.y - c.y)) / det
      val beta  = ((c.y - a.y) * (p.x - c.x) + (a.x - c.x) * (p.y - c.y)) / det
      val gamma = 1.0f - alpha - beta
      (alpha >= 0) && (beta >= 0) && (gamma >= 0)
    }
  }

  /**
   * Quadrilateral class that can test whether points are within its periphery.
   *
   * The points should be defined in a clockwise or counter-clockwise order around the periphery.
   *
   * @param a corner of quad
   * @param b corner of quad
   * @param c corner of quad
   * @param d corner of quad
   */
  case class Quadrilateral(a: Vec2D, b: Vec2D, c: Vec2D, d: Vec2D) {
    private val tri1 = Triangle(a, b, c)
    private val tri2 = Triangle(a, c, d)
    def within(p: Vec2D): Boolean = tri1.within(p) || tri2.within(p)
  }

}
