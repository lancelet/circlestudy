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

object Geom {

  final case class Vec2D(x: Float, y: Float) {
    def -(v: Vec2D): Vec2D = Vec2D(x - v.x, y - v.y)
    def *(s: Float): Vec2D = Vec2D(x * s, y * s)
    def dot(v: Vec2D): Float = x * v.x + y * v.y
    def angle: Float = atan2(y, x).toFloat
    def mag: Float = sqrt(x * x + y * y).toFloat
  }
  object Vec2D {
    def fromVec3Dxy(v3: Vec3D): Vec2D = Vec2D(v3.x, v3.y)
    def fromPolar(r: Float, theta: Float): Vec2D = Vec2D((r * cos(theta)).toFloat, (r * sin(theta)).toFloat)
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
    val initialGuess = new InitialGuess(Array(pt3circle.origin.x, pt3circle.origin.y, pt3circle.radius)) // x,y,r
    
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
  
}