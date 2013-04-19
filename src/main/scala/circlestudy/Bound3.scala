package circlestudy

trait Bound3 {
  def min: Vec3
  def max: Vec3

  require(min.x <= max.x, "minimum x must be <= maximum x")
  require(min.y <= max.y, "minimum y must be <= maximum y")
  require(min.z <= max.z, "minimum z must be <= maximum z")
}

object Bound3 {
  def apply(min: Vec3, max: Vec3): Bound3 = Bound3Default(min, max)
  def apply(minx: Double, maxx: Double, miny: Double, maxy: Double, minz: Double, maxz: Double): Bound3 =
    Bound3Default(Vec3(minx, miny, minz), Vec3(maxx, maxy, maxz))
}

private [this] case class Bound3Default(min: Vec3, max: Vec3) extends Bound3
