package circlestudy.trials

import java.io.File
import scala.collection.immutable._
import scalaz.Validation

import circlestudy.Vec3
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

}

object Trial {
  def fromTRCFile(trcFile: File): Validation[String, Trial] = MocapUtilsTrial.fromTRCFile(trcFile)
}
