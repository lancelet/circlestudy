package circlestudy

/** 3D Double-precision vector. */
trait Vec3 {
  def x: Double
  def y: Double
  def z: Double

  override def equals(obj: Any): Boolean = if (obj.isInstanceOf[Vec3]) {
    val v = obj.asInstanceOf[Vec3]
    (v.x == x) && (v.y == y) && (v.z == z)
  } else false

  override def hashCode(): Int = x.hashCode * 41 * 41 + y.hashCode * 41 + z.hashCode
}

object Vec3 {
  def apply(x: Double, y: Double, z: Double): Vec3 = Vec3Default(x, y, z)
}

/** Default (case class) implementation of a Vec3. */
private[this] case class Vec3Default(x: Double, y: Double, z: Double) extends Vec3
