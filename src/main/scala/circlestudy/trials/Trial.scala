package circlestudy.trials

import java.io.File
import scala.collection.immutable._
import scalaz.Validation

import circlestudy.trials.mocaputils.MocapUtilsTrial

/**
 * Trial trait for the circle study.
 */
trait Trial {

  def markers: IndexedSeq[MarkerWithGaps]

  /** Marker, which may have gaps in its sampling. */
  trait MarkerWithGaps {
    /** Name of the marker. */
    def name: String
    /** Coordinates. */
    def co: IndexedSeq[Option[Vec3]]
    /** Sampling frequency. */
    def sampleFreq: Double

    /** `x` coordinates */
    def xs: IndexedSeq[Option[Double]]
    /** `y` coordinates */
    def ys: IndexedSeq[Option[Double]]
    /** `z` coordinates */
    def zs: IndexedSeq[Option[Double]]
  }

  /** 3D vector. */
  trait Vec3 {
    def x: Double
    def y: Double
    def z: Double

    override def equals(obj: Any): Boolean = {
      if (obj.isInstanceOf[Vec3]) {
        val v = obj.asInstanceOf[Vec3]
        (v.x == x) && (v.y == y) && (v.z == z)
      } else false
    }

    override def hashCode(): Int = SimpleVec3(x, y, z).hashCode
  }
  object Vec3 {
    def apply(x: Double, y: Double, z: Double): Vec3 = SimpleVec3(x, y, z)
  }
  private case class SimpleVec3(x: Double, y: Double, z: Double) extends Vec3

}

object Trial {
  def fromTRCFile(trcFile: File): Validation[String, Trial] = MocapUtilsTrial.fromTRCFile(trcFile)
}
