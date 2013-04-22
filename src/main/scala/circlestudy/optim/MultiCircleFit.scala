package circlestudy.optim

import math.sqrt
import scala.collection.immutable._
import org.apache.commons.math3.optim.nonlinear.scalar.ObjectiveFunction
import org.apache.commons.math3.analysis.MultivariateFunction
import org.apache.commons.math3.optim.nonlinear.scalar.noderiv.BOBYQAOptimizer
import org.apache.commons.math3.optim.MaxEval
import org.apache.commons.math3.optim.InitialGuess
import org.apache.commons.math3.optim.nonlinear.scalar.GoalType
import org.apache.commons.math3.optim.PointValuePair
import org.apache.commons.math3.optim.SimpleBounds

object MultiCircleFit {

  /** A marker for the purposes of circle fitting. */
  trait Marker {
    /** 
     * Returns an array of (x,y) coordinates of marker sample positions.  The array is organized as
     * (x1, y1, x2, y2, ..., xn, yn).  */
    def co2XY: Array[Double]
  }

  /** 
   * Computes the error for one marker.
   * 
   * @param marker marker
   * @param xc x coordinate of the circle center
   * @param yc y coordinate of the circle center
   * @param r  radius of the circle
   */
  private [this] def markerError(marker: Marker, xc: Double, yc: Double, r: Double): Double = {
    val co: Array[Double] = marker.co2XY
    assert(co.length % 2 == 0, "length of coordinate array must be an even number")
    val Ni: Double = (co.length / 2).toDouble
    var accum: Double = 0.0
    var i: Int = 0
    while (i < co.length) {  // while loop for speed
      val dx = co(i)   - xc
      val dy = co(i+1) - yc
      i = i + 2  // jump over current x, y coordinates
      val delta = sqrt(dx * dx + dy * dy) - r
      accum = accum + (delta * delta)
    }
    accum / Ni
  }
  
  
  def fit(markers: IndexedSeq[Marker], weights: IndexedSeq[Double]): (Double, Double, IndexedSeq[Double]) = {
    require(markers.length == weights.length, "one weight must be provided for each marker")
    
    // Objective function to minimize
    val objectiveFn = new ObjectiveFunction(
      new MultivariateFunction {
        def value(p: Array[Double]): Double = {
          // p is laid out as (xc, yc, r1, r2, ..., rn)
          val xc = p(0)
          val yc = p(1)
          val errors = for (i <- 0 until markers.length) yield {
            val marker = markers(i)
            val w      = weights(i)
            val r      = p(i + 2)
            markerError(marker, xc, yc, r) * w
          }
          errors.sum / (weights.sum.toDouble)
        }
      }
    )
    
    // Optimizer
    val optimizer = new BOBYQAOptimizer(markers.length + 2 + 2)
    val maxEval = new MaxEval(500 * markers.length)
    val initialGuess = new InitialGuess((Seq(0.0, 0.0) ++: Seq.fill(markers.length)(1000.0)).toArray)
    val goal: GoalType = GoalType.MINIMIZE
    val bounds: SimpleBounds = SimpleBounds.unbounded(markers.length + 2)
    
    // Do optimization
    val pvp: PointValuePair = optimizer.optimize(objectiveFn, initialGuess, maxEval, goal, bounds)
    
    // Extract values
    val p: Array[Double] = pvp.getPoint()
    val xc = p(0)
    val yc = p(1)
    val rs = p.toIndexedSeq.drop(2)
    
    // Return (xc, yc, rs)
    (xc, yc, rs)
  }
  
}