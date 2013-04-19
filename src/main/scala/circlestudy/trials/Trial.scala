package circlestudy.trials

import java.io.File
import scala.collection.immutable._
import scalaz.Validation

import circlestudy.{Bound3, Vec3}
import circlestudy.trials.mocaputils.MocapUtilsTrial

/**
 * Trial trait for the circle study.
 */
trait Trial {

  import Trial._
  
  def markers: IndexedSeq[MarkerWithGaps]

  def bounds: Bound3 = Bound3(markers map(_.bounds) withFilter(_.isDefined) map(_.get))
  
}

object Trial {
  def fromTRCFile(trcFile: File): Validation[String, Trial] = MocapUtilsTrial.fromTRCFile(trcFile)

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

    /** Marker exists if some coordinates are present */
    def exists: Boolean
    
    /** Bounding box (does not exist if the marker is totally empty) */
    def bounds: Option[Bound3]
  }
}
