package circlestudy

trait Bound2 {
  def min: Vec2
  def max: Vec2
  
  require(min.x <= max.x, "minimum x must be <= maximum x")
  require(min.y <= max.y, "minimum y must be <= maximum y")
}

object Bound2 {
  def apply(min: Vec2, max: Vec2): Bound2 = Bound2Default(min, max)
  def apply(minx: Double, maxx: Double, miny: Double, maxy: Double): Bound2 = 
    Bound2Default(Vec2(minx, miny), Vec2(maxx, maxy))
}

private [this] case class Bound2Default(min: Vec2, max: Vec2) extends Bound2