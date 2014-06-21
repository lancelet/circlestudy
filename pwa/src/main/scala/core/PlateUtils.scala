package core

import scala.collection.immutable._

import c3d.{Vec3D, ForcePlate, C3D, Point}
import pwa.Geom.{Vec2D, Quadrilateral}


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
  assert(range > 0, "range must be > 0")
  def fraction(f: Float): Int = on + (range * f).toInt
  lazy val percent20: Int = fraction(0.2f)
  lazy val percent80: Int = fraction(0.8f)
}


object PlateUtils {

  implicit class RichPoint(pt: Point) {

    /**
     * Checks whether a point is within the z-projection of a plate for an entire contact interval.
     *
     * @param interval interval to check
     * @return true if the point lies within the projection of the plate boundary
     */
    def withinPlateForWholeInterval(interval: ContactInterval): Boolean =
      (interval.on until interval.off).forall(interval.plate.withinZProjection(pt, _))

  }

  implicit class RichPlate(plate: ForcePlate) {

    /**
     * For a force plate, returns a `Quadrilateral` which represents its boundary
     *
     * @return quadrilateral boundary of the force plate
     */
    def quadForPlateZProjection: Quadrilateral = {
      val ps = plate.corners.map { c: Vec3D => Vec2D(c.x, c.y) }
      Quadrilateral(ps(0), ps(1), ps(2), ps(3))
    }

    /**
     * Checks if a point lies within the z-projection of a force plate boundary at a given index.
     *
     * @param p point to check
     * @param ptIndex index at which to check the point's position
     * @return true if the point lies within the z-projection of the plate boundary at the sample
     */
    def withinZProjection(p: Point, ptIndex: Int): Boolean =
      p(ptIndex)
        .map(Vec2D.fromVec3Dxy)
        .fold(false)(quadForPlateZProjection.within)

  }

  implicit class RichC3D(c3d: C3D) {

    /**
     * Finds the factor required to convert from the sample rate of force platforms to the sample
     * rate of 3D points.
     *
     * This is the force platform sampling rate divided by the point sampling rate.
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
     * Converts a point sample to a force plate sample.
     *
     * @param s sample to convert
     * @return sample converted
     */
    def ptToFp(s: Int): Int = (s.toDouble * fpToPtFactor.toDouble).toInt

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

          if (on == -1 || off == -1) cs.reverse
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
