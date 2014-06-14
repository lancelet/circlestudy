package core

import scala.collection.immutable._

import pwa.Geom.Vec2D
import c3d.{Point, ForcePlate, C3D}

import DataStore.{ RichC3D => DataStoreRichC3D }


/** Limb of the horse */
sealed trait Limb {
  import Limb._
  def isForelimb: Boolean = (this == LF) || (this == RF)
}
object Limb {
  object LF extends Limb { override def toString = "LF" }
  object RF extends Limb { override def toString = "RF" }
  object LH extends Limb { override def toString = "LH" }
  object RH extends Limb { override def toString = "RH" }
  def fromString(s: String): Limb = s.toUpperCase match {
    case "LF" => LF
    case "LH" => LH
    case "RF" => RF
    case "RH" => RH
    case _ => throw new IllegalArgumentException(s"limb '$s' is invalid")
  }
}

/** A single footfall of a horse. */
trait Footfall {
  import Direction._
  import Limb._
  def direction: Direction
  def gait: Gait
  def forceWeightedPWA: Vec2D
  def limb: Limb
  def interval: ContactInterval
  def plateNumber: Int
  def c3d: C3D
  def isOuterLimb: Boolean = direction match {
    case CircleLeft  => (limb == RF) || (limb == RH)
    case CircleRight => (limb == LF) || (limb == LH)
    case Straight    => false
  }
}

/**
 * A single contact interval of a force plate.
 *
 * @param on C3D point sample at which contact starts (point sample, NOT force plate sample)
 * @param off C3D point sample at which contact stops (point sample, NOT force plate sample)
 * @param plate force plate on which the contact occurred
 */
case class ContactInterval (on: Int, off: Int, plate: ForcePlate) {
  require(off > on, s"ContactInterval requires off ($off) to be > on ($on)")
  def range: Int = off - on
  def fraction(f: Float): Int = on + (range * f).toInt
  lazy val percent20: Int = fraction(0.2f)
  lazy val percent80: Int = fraction(0.8f)
}


object HoofUtils {

  implicit class RichC3D(c3d: C3D) {

    /**
     * Finds the factor required to convert from the sample rate of force platforms to the sample
     * rate of 3D points.
     *
     * @return factor to divide force plate samples by in order to obtain 3D point samples
     */
    def fpToPtFactor: Int = (c3d.platforms.rate / c3d.points.rate).toInt

    /**
     * Converts a force plate sample to a point sample.
     *
     * Force plates and 3D points can be sampled at different rates.  This method converts from the
     * force plate sample numbers to 3D samples.
     *
     * @param s sample to convert
     * @return sample converted
     */
    def fpToPt(s: Int): Int = (s.toDouble / fpToPtFactor.toDouble).toInt

    /**
     * Finds all contact intervals that occur on a force plate in a given C3D trial.
     *
     * Contact intervals occur from when the force first exceeds the `forceThreshold`, until the
     * time at which the force drops below the `forceThreshold`.
     *
     * @param forceThreshold threshold value to trigger contact intervals
     * @param minDur minimum duration of acceptable contacts, in real time
     * @return contact intervals
     */
    def contactIntervals(forceThreshold: Float, minDur: Float): Seq[ContactInterval] = {

      def contactIntervalsForPlate(plate: ForcePlate): Seq[ContactInterval] = {
        val f: Seq[Float] = plate.force.resampleByAveraging(fpToPtFactor).map(_.mag)

        // traverse forward through the plate's samples, accumulating stance phases
        @scala.annotation.tailrec
        def go(cs: List[ContactInterval], idx: Int): List[ContactInterval] = {
          val on  = f.indexWhere(_ >= forceThreshold, idx)
          val off = f.indexWhere(_ <  forceThreshold, on)

          if (on == -1 || off == 1) cs.reverse
          else go(ContactInterval(on, off, plate) :: cs, off)
        }

        go(Nil, 0)
      }

      val minSamples = minDur * c3d.points.rate

      c3d.platforms.plates
        .flatMap(contactIntervalsForPlate)
        .filter(_.range >= minSamples)
    }

  }


}
