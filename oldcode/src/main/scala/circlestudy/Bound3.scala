package circlestudy

import scala.collection.immutable._

trait Bound3 {
  def min: Vec3
  def max: Vec3

  require(min.x <= max.x, "minimum x must be <= maximum x")
  require(min.y <= max.y, "minimum y must be <= maximum y")
  require(min.z <= max.z, "minimum z must be <= maximum z")
  
  def toBound2: Bound2 = Bound2(min.x, max.x, min.y, max.y)
}

object Bound3 {
  def apply(min: Vec3, max: Vec3): Bound3 = Bound3Default(min, max)
  def apply(minx: Double, maxx: Double, miny: Double, maxy: Double, minz: Double, maxz: Double): Bound3 =
    Bound3Default(Vec3(minx, miny, minz), Vec3(maxx, maxy, maxz))

  def apply(boundSeq: Seq[Bound3]): Bound3 = {
    require(boundSeq.length >= 1, "boundSeq must contain at least 1 bound")
    val bh = boundSeq.head
    val a: Array[Double] = Array(bh.min.x, bh.max.x, bh.min.y, bh.max.y, bh.min.z, bh.max.z)
    def includePointLocal(p: Vec3) {
      if (p.x < a(0)) a(0) = p.x
      if (p.x > a(1)) a(1) = p.x
      if (p.y < a(2)) a(2) = p.y
      if (p.y > a(3)) a(3) = p.y
      if (p.z < a(4)) a(4) = p.z
      if (p.z > a(5)) a(5) = p.z
    }
    for (bound <- boundSeq.tail) {
      includePointLocal(bound.min)
      includePointLocal(bound.max)
    }
    Bound3(a(0), a(1), a(2), a(3), a(4), a(5))
  }
}

private [this] case class Bound3Default(min: Vec3, max: Vec3) extends Bound3
