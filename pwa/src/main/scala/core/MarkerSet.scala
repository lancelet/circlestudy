package core

import scala.collection.immutable._

import c3d.{Point, C3D}
import core.DataStore.{ RichC3D => DataStoreRichC3D }


object MarkerSet {

  final val motionHoofMarkerNames = IndexedSeq(
    "HoofLatBottom",
    "Heel",
    "HoofLatTop",
    "HoofDorsal"
  )

  implicit class RichC3D(c3d: C3D) {

    /**
     * Finds the points on the hoof of a limb that should be present in a motion trial.
     *
     * @param limb limb for which to find the hoof points
     * @return points that should be present on the hoof during a motion trial
     */
    def motionHoofPointsForLimb(limb: Limb): IndexedSeq[Point] =
      motionHoofMarkerNames
        .map { base => s"${limb.toString}${base}" }
        .map { name => DataStoreRichC3D(c3d).getCSPoint(name).get }

  }


}
