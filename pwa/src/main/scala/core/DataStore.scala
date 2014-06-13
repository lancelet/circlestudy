package core

import java.io.File
import c3d.C3D

/** Single motion (non-static) trial. */
trait MotionTrial {
  def direction: Direction
  def gait: Gait
  def c3d: C3D
}

/** Direction of a trial: straight, circle right or circle left. */
sealed trait Direction {
  import Direction._
  def isCircle: Boolean = (this == CircleLeft) || (this == CircleRight)
}
object Direction {
  case object Straight    extends Direction { override def toString = "straight"     }
  case object CircleRight extends Direction { override def toString = "circle right" }
  case object CircleLeft  extends Direction { override def toString = "circle left"  }
}

/** Gait for a trial: walk or trot. */
sealed trait Gait
object Gait {
  case object Walk extends Gait { override def toString = "walk" }
  case object Trot extends Gait { override def toString = "trot" }
}

/**
 * Represents the total store of data for the project.
 *
 * @param dataDir  top-level data directory
 */
class DataStore(dataDir: File = new File("/Users/jsm/Documents/dev/circlestudy/data")) {

}


