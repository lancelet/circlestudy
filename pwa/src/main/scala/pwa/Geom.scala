package pwa

import scala.annotation.tailrec
import scala.collection.immutable._
import math.sqrt
import org.apache.commons.math3.analysis.MultivariateFunction
import org.apache.commons.math3.optim.InitialGuess
import org.apache.commons.math3.optim.MaxEval
import org.apache.commons.math3.optim.PointValuePair
import org.apache.commons.math3.optim.SimpleBounds
import org.apache.commons.math3.optim.nonlinear.scalar.GoalType
import org.apache.commons.math3.optim.nonlinear.scalar.ObjectiveFunction
import org.apache.commons.math3.optim.nonlinear.scalar.noderiv.BOBYQAOptimizer

object Geom {

  type Vec2D = (Float, Float)
  
  final case class Circle(origin: Vec2D, radius: Float)
  
  final case class Line(origin: Vec2D, vec: Vec2D)
  
  def distanceBetweenPoints(p1: Vec2D, p2: Vec2D): Float = {
    val dx: Float = p2._1 - p1._1
    val dy: Float = p2._2 - p1._2
    sqrt(dx * dx + dy * dy).toFloat
  }  
  
  def lineIntersection(l1: Line, l2: Line): Vec2D = {
    val det: Float = l1.vec._1 * l2.vec._2 - l2.vec._1 * l1.vec._2
    val c1: Float = -l1.vec._2 / det
    val c2: Float =  l1.vec._1 / det
    val dox: Float = l1.origin._1 - l2.origin._1
    val doy: Float = l1.origin._2 - l2.origin._2
    val f: Float = c1 * dox + c2 * doy
    val x: Float = l2.origin._1 + f * l2.vec._1
    val y: Float = l2.origin._2 + f * l2.vec._2
    (x, y)
  }
  
  def circleThrough3Points(p1: Vec2D, p2: Vec2D, p3: Vec2D): Circle = {
    val bisector1: Line = perpendicularBisector(p1, p2)
    val bisector2: Line = perpendicularBisector(p2, p3)
    val origin: Vec2D = lineIntersection(bisector1, bisector2)
    val radius: Float = distanceBetweenPoints(origin, p2)
    Circle(origin, radius)
  }
  
  def perpendicularBisector(p1: Vec2D, p2: Vec2D): Line = {
    val origin_x: Float = (p1._1 + p2._1) / 2.0f
    val origin_y: Float = (p1._2 + p2._2) / 2.0f
    val dx: Float = p2._1 - origin_x
    val dy: Float = p2._2 - origin_y
    val vecx: Float = -dy
    val vecy: Float = +dx
    val mag: Float = sqrt(vecx * vecx + vecy * vecy).toFloat
    Line((origin_x, origin_y), (vecx / mag, vecy / mag))
  }  
  
  def lsqCircle(xx: IndexedSeq[Vec2D]): Circle = {
    
    // find initial guess from 3 points
    val pt3circle = circleThrough3Points(xx(0), xx(xx.length / 2), xx.last)
    val initialGuess = new InitialGuess(Array(pt3circle.origin._1, pt3circle.origin._2, pt3circle.radius)) // x,y,r
    
    // general multi-variate optimisation
    val fitFunction: MultivariateFunction = new MultivariateFunction {
      def value(xyr: Array[Double]): Double = {
        @tailrec def accum(err: Double, index: Int): Double = {
          if (index == xx.length) {
            err
          } else {
            val v: Vec2D = xx(index)
            val dx = v._1 - xyr(0)            // x coordinate relative to current origin
            val dy = v._2 - xyr(1)            // y coordinate relative to current origin
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
    
    Circle((x, y), r)
    
  }
  
}