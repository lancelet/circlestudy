package pwa

import math.sqrt

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
  
}