package circlestudy.trials.mocaputils

import java.io.File
import scalaz.Validation

import circlestudy.trials.Trial
import mocaputils.{GappedMarker, TRCData, TRCReader, Vec3 => MUVec3}
import scala.collection.immutable.IndexedSeq

/**
 * A Trial implemented by MocapUtils.
 */
class MocapUtilsTrial(trcData: TRCData) extends Trial {

  /** Adapts a mocaputils.Vec3 as a circlestudy.trials.Trial.Vec3 */
  private class MCUVec3(v: MUVec3) extends Vec3 {
    def x: Double = v.x
    def y: Double = v.y
    def z: Double = v.z
  }

  /** Adapts a mocaputils.GappedMarker as a circlestudy.trials.Trial.MarkerWithGaps */
  private class MCUMarkerWithGaps(marker: GappedMarker) extends MarkerWithGaps {
    def name: String = marker.name
    def co: IndexedSeq[Option[Vec3]] = new IndexedSeq[Option[Vec3]] {
      def length: Int = marker.co.length
      def apply(idx: Int): Option[Vec3] = marker.co(idx).map(new MCUVec3(_))
    }
    def sampleFreq: Double = marker.fs
    def xs: IndexedSeq[Option[Double]] = marker.xs
    def ys: IndexedSeq[Option[Double]] = marker.ys
    def zs: IndexedSeq[Option[Double]] = marker.zs
  }

  def markers: IndexedSeq[MarkerWithGaps] = trcData.markers.map(new MCUMarkerWithGaps(_))

}

object MocapUtilsTrial {
  def fromTRCFile(trcFile: File): Validation[String, MocapUtilsTrial] = {
    TRCReader.read(trcFile.getAbsolutePath).map(new MocapUtilsTrial(_))
  }
}
