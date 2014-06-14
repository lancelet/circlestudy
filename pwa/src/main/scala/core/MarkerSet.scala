package core

import scala.collection.immutable._

import c3d.{Vec3D, Point, C3D}
import core.DataStore.{ RichC3D => DataStoreRichC3D }
import core.PlateUtils.{ RichC3D => PlateUtilsRichC3D, RichPoint }
import pwa.Geom._
import c3d.util.transform.{RotationMatrix, XForm, VirtualPoint}
import pwa.Geom.Rot2D


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
 * Instantaneous configuration of the hoof coordinate system within the world coordinate system.
 */
trait HoofCoordsInWorld {
  def originInWorld: Vec3D
  def xVecInWorld: Vec3D
  def yVecInWorld: Vec3D
  def worldToHoof: XForm = {
    val r = RotationMatrix.fromBasisVectorsXY(xVecInWorld, yVecInWorld)
    new XForm { def apply(v: Vec3D): Vec3D = r(v - originInWorld) }
  }
}


/**
 * Operations specific to the marker set.
 */
object MarkerSet {

  final val motionHoofMarkerNames = IndexedSeq(
    "HoofLatBottom",
    "Heel",
    "HoofLatTop",
    "HoofDorsal"
  )

  final val virtualHoofMarkerNames = IndexedSeq(
    "HoofLatHeel",
    "HoofLatquarter",
    "HoofToe",
    "HoofMedquarter",
    "HoofMedHeel"
  )

  /**
   * Finds the peak force that occurs within a given footfall.
   *
   * @param footfall footfall
   * @return peak force
   */
  def peakForce(footfall: Footfall): Float = {
    val sta = PlateUtilsRichC3D(footfall.c3d).ptToFp(footfall.interval.on)
    val end = PlateUtilsRichC3D(footfall.c3d).ptToFp(footfall.interval.off)
    footfall.interval.plate
      .force
      .iterator
      .slice(sta, end)
      .map(_.mag)
      .max
  }

  implicit class RichC3D(c3d: C3D) {

    /**
     * Finds the hoof points for all limbs.
     *
     * This method should be called on a motion C3D file.
     *
     * Real points are the hoof points defined in each motion trial.  Virtual points are the
     * points in the static trial, projected into the dynamic trial using the transformations
     * obtained from the real points.
     *
     * @param static static trial for the horse
     * @return hoof points
     */
    def hoofPointsForAllLimbs(static: C3D): Seq[Point] =
      Seq(Limb.LF, Limb.RF, Limb.LH, Limb.RH)
        .flatMap(hoofPointsForLimb(_, static))

    /**
     * Finds all points (both real and virtual) that should be present on the hoof of a given
     * limb.
     *
     * Real points are the hoof points defined in each motion trial.  Virtual points are the
     * points in the static trial, projected into the dynamic trial using the transformations
     * obtained from the real points.
     *
     * This method should be called on a motion C3D trial.
     *
     * @param limb limb for which to find the points
     * @param static static trial for the horse
     * @return all hoof points
     */
    def hoofPointsForLimb(limb: Limb, static: C3D): Seq[Point] =
      motionHoofPointsForLimb(limb) ++ virtualHoofPointsForLimb(limb, static)

    /**
     * Finds the points on the hoof of a limb that should be present in a motion trial.
     *
     * This should be called on a motion C3D trial.
     *
     * @param limb limb for which to find the hoof points
     * @return points that should be present on the hoof during a motion trial
     */
    def motionHoofPointsForLimb(limb: Limb): IndexedSeq[Point] =
      motionHoofMarkerNames
        .map(nameOfHoofMarkerOnLimb(limb, _))
        .map(DataStoreRichC3D(c3d).getCSPoint(_).get)


    /**
     * Finds the points on the hoof of a limb that should be present in a static trial.
     *
     * This should be called on a static C3D trial.
     *
     * @param limb limb for which to find the hoof points
     * @return points that should be present on the hoof during a static trial
     */
    def staticHoofPointsForLimb(limb: Limb): IndexedSeq[Point] =
      virtualHoofMarkerNames
        .map(nameOfHoofMarkerOnLimb(limb, _))
        .map(DataStoreRichC3D(c3d).getCSPoint(_).get)

    /**
     * Computes the virtual hoof points that should be present on a limb.
     *
     * Virtual points in this case are the projections of the static hoof points into the dynamic
     * trials, with the transformations calculated from the dynamic hoof markers.
     *
     * This method should be called on a motion C3D trial.
     *
     * @param limb limb for which to find the virtual hoof points
     * @param static static trial for the horse
     * @return virtual hoof points
     */
    def virtualHoofPointsForLimb(limb: Limb, static: C3D): Seq[Point] = {
      val motionPts = c3d.motionHoofPointsForLimb(limb)
      val staticPts = static.staticHoofPointsForLimb(limb)
      val staticRefs = staticPts.map(averagePt)

      (staticPts zip staticRefs)
        .map { case (staticPt, staticRef) =>
          VirtualPoint(staticPt.name, "", staticRef, staticRefs, motionPts)
        }
        .map { new MemoizedPoint(_) }
    }

    /**
     * Optionally return the limb that is over the plate for an entire contact interval.
     *
     * This method should be called on a motion C3D trial.
     *
     * @param static static trial for the horse
     * @param interval contact interval
     * @return optional limb
     */
    def limbForContactInterval(static: C3D, interval: ContactInterval): Option[Limb] = {
      c3d.closestLimbAtImpact(interval).flatMap { closest: Limb =>
        val hoofMarkers: Seq[Point] = c3d.hoofPointsForLimb(closest, static)
        if (hoofMarkers.forall(_.withinPlateForWholeInterval(interval))) Some(closest) else None
      }
    }

    /**
     * Finds the closest limb to a plate at impact for a given contact interval.
     *
     * The dorsal markers on the hooves are used for this comparison.
     *
     * @param interval contact interval
     * @return closest limb
     */
    def closestLimbAtImpact(interval: ContactInterval): Option[Limb] = {

      val onPt: Vec3D = interval.plate.pwa(PlateUtilsRichC3D(c3d).ptToFp(interval.on))

      def distanceAtImpact(p: Point): Option[Float] = p(interval.on) map { pt: Vec3D =>
        (pt - onPt).mag
      }

      val limbMarkerNames: List[(Limb, String)] = List(
        Limb.RF -> "RFHoofDorsal",
        Limb.LF -> "LFHoofDorsal",
        Limb.RH -> "RHHoofDorsal",
        Limb.LH -> "LHHoofDorsal"
      )
      val minLimb: List[(Limb, Float)] = limbMarkerNames
        .map    { case (limb, name)     => limb -> DataStoreRichC3D(c3d).getCSPoint(name).get }
        .map    { case (limb, point)    => limb -> distanceAtImpact(point)                    }
        .filter { case (_   , opt)      => opt.isDefined                                      }
        .map    { case (limb, distance) => limb -> distance.get                               }
        .sortBy { case (limb, distance) => distance                                           }
      minLimb match {
        case (limb, _) :: _ => Some(limb)
        case _              => None
      }

    }

    implicit class RichVec3D(v: Vec3D) {

      /** Projects a vector to z=0 */
      def projectToZ: Vec3D = Vec3D(v.x, v.y, 0)

    }

    /**
     * Finds the average world positioning of the hoof coordinate system for a particular contact
     * interval.
     *
     * The hoof coordinate system is projected to the z = 0 plane.
     *
     * @param static static trial
     * @param interval interval for which to find the hoof positioning
     * @return average world-space positioning of the hoof coordinate system
     */
    def hoofCoordsForInterval(static: C3D, interval: ContactInterval): Option[HoofCoordsInWorld] = {
      limbForContactInterval(static, interval).flatMap { limb: Limb =>
        val hoofPts = c3d.hoofPointsForLimb(limb, static)
        def middleAvg(ptName: String): Option[Vec3D] =
          hoofPts
            .find(_.name.contains(ptName))
            .map(averagePt(_, interval.percent20, interval.percent80))
        for {
          med <- middleAvg("HoofMedquarter")
          lat <- middleAvg("HoofLatquarter")
          dor <- middleAvg("HoofToe")
        } yield new HoofCoordsInWorld {
          val originInWorld: Vec3D = ((med + lat) / 2.0f).projectToZ
          val yVecInWorld: Vec3D   = (dor - originInWorld).projectToZ.asUnit
          val xVecInWorld: Vec3D   = Rot2D(math.toRadians(-90)).asRotationMatrix(yVecInWorld)
        }
      }
    }

    /**
     * Finds a circle through the path of T6.
     *
     * The circle is found from 3 points.  These are the first and last positions at which the T6
     * marker is defined, and a point close to the middle where the marker is also defined.
     *
     * @return circle
     */
    def t6CircleThrough3Points: Circle = {
      val t6 = DataStoreRichC3D(c3d).getCSPoint("T6").get

      val sta = t6.indexWhere(_.isDefined)
      val end = t6.lastIndexWhere(_.isDefined)
      val mid = t6.indexWhere(_.isDefined, (sta + end) / 2)

      circleThrough3Points(
        Vec2D.fromVec3Dxy(t6(sta).get),
        Vec2D.fromVec3Dxy(t6(mid).get),
        Vec2D.fromVec3Dxy(t6(end).get)
      )
    }

    /**
     * Finds the average speed of the T6 marker for a trial.
     *
     * In the case of a straight line trial, this is done by comparing the end and start marker
     * positions.  For a circular trial, the subtended angle is calculated and the radius of a
     * T6 marker circle is used.
     *
     * @param direction direction of the trial
     * @return T6 marker speed
     */
    def t6AverageSpeed(direction: Direction): Float = {

      val t6 = DataStoreRichC3D(c3d).getCSPoint("T6").get
      val sta = t6.indexWhere(_.isDefined)
      val end = t6.lastIndexWhere(_.isDefined)
      val pSta = t6(sta).get
      val pEnd = t6(end).get
      val dT = (end - sta) / c3d.points.rate

      def straight: Float = (pEnd - pSta).mag / dT

      def circles: Float = {
        val circle = c3d.t6CircleThrough3Points
        val aSta = Vec2D.fromVec3Dxy(pSta).angle
        val aEnd = Vec2D.fromVec3Dxy(pEnd).angle
        (aEnd - aSta) * circle.radius / dT
      }

      if (direction.isCircle) circles else straight

    }

    /**
     * Computes the force-weighted PWA (point of wrench application) in world coordinates.
     *
     * @param interval contact interval
     * @return force-weighted PWA in world coordinates
     */
    def forceWeightedPWADuringContact(interval: ContactInterval): Vec3D = {
      val staForceIndex = PlateUtilsRichC3D(c3d).ptToFp(interval.on)
      val endForceIndex = PlateUtilsRichC3D(c3d).ptToFp(interval.off)

      val mag = interval.plate.force.slice(staForceIndex, endForceIndex).map(_.mag)
      val pwa = interval.plate.pwa.slice(staForceIndex, endForceIndex)
      assert(mag.length == pwa.length)

      def notNan(v: Vec3D): Boolean = (!v.x.isNaN) && (!v.y.isNaN) && (!v.z.isNaN)

      val pwaWSum = (pwa zip mag)
        .map { case (p: Vec3D, m: Float) => if (notNan(p)) p * m else Vec3D(0,0,0) }  // TODO: ?
        .reduce ( _ + _ )
      pwaWSum / mag.sum
    }

    /**
     * Finds the force-weighted PWA (point of wrench application), and transforms it into the
     * hoof-local coordinate system.
     *
     * @param static static C3D trial for the horse
     * @param interval contact interval
     * @return 2D vector in the hoof-local coordinate system
     */
    def forceWeightedPWAInHoofCoords(static: C3D, interval: ContactInterval): Vec2D = {
      val worldToHoof = c3d.hoofCoordsForInterval(static, interval).get.worldToHoof
      val weightedPWA = forceWeightedPWADuringContact(interval)
      Vec2D.fromVec3Dxy(worldToHoof(weightedPWA))
    }

  }

  /** Suffixes the name of a hoof marker to a limb identifier. */
  private def nameOfHoofMarkerOnLimb(limb: Limb, suffix: String) = s"${limb.toString}${suffix}"

}
