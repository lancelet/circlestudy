package circlestudy

/** 2D Double-precision vector. */
trait Vec2 {
  def x: Double
  def y: Double
  
  override def equals(obj: Any): Boolean = if (obj.isInstanceOf[Vec2]) {
    val v = obj.asInstanceOf[Vec2]
    (v.x == x) && (v.y == y)
  } else false

  override def hashCode(): Int = x.hashCode * 41 + y.hashCode  
}

object Vec2 {
  def apply(x: Double, y: Double): Vec2 = Vec2Default(x, y)
}

/** Default (case class) implementation of a Vec3. */
private[this] case class Vec2Default(x: Double, y: Double) extends Vec2
