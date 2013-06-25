package circlestudy

trait Bound2 {
  def min: Vec2
  def max: Vec2
  
  def width: Double  = max.x - min.x
  def height: Double = max.y - min.y
  
  require(min.x <= max.x, "minimum x must be <= maximum x")
  require(min.y <= max.y, "minimum y must be <= maximum y")
  
  def addVec2(v: Vec2): Bound2 = Bound2(
    Vec2(if (v.x < min.x) v.x else min.x, if (v.y < min.y) v.y else min.y),
    Vec2(if (v.x > max.x) v.x else max.x, if (v.y > max.y) v.y else max.y)
  )
  def addBound2(b: Bound2): Bound2 = addVec2(b.min).addVec2(b.max)
}

object Bound2 {
  def apply(min: Vec2, max: Vec2): Bound2 = Bound2Default(min, max)
  def apply(minx: Double, maxx: Double, miny: Double, maxy: Double): Bound2 = 
    Bound2Default(Vec2(minx, miny), Vec2(maxx, maxy))
}

private [this] case class Bound2Default(min: Vec2, max: Vec2) extends Bound2