package core

import scala.collection.immutable._

import c3d.{Point, C3D}
import core.DataStore.{ RichC3D => DataStoreRichC3D }
import pwa.Geom.{MemoizedPoint, averagePt}
import c3d.util.transform.VirtualPoint


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

  implicit class RichC3D(c3d: C3D) {

    /**
     * Finds the hoof points for all limbs.
     *
     * This method should be called on a motion C3D file.
     *
     * (See hoofPointsForLimb for a description of the points returned.)
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

  }

  private def nameOfHoofMarkerOnLimb(limb: Limb, suffix: String) = s"${limb.toString}${suffix}"

}
