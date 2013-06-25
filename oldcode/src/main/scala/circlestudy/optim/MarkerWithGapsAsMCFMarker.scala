package circlestudy.optim

import scala.collection.immutable._
import circlestudy.trials.Trial.MarkerWithGaps
import circlestudy.{Vec2, Vec3}

/** Represents a Trial.MarkerWithGaps as a MultiCircleFit.Marker. */
final class MarkerWithGapsAsMCFMarker(m: MarkerWithGaps, takeEvery: Int) extends MultiCircleFit.Marker {
  lazy val co2XY: Array[Double] = {
    m.co.filter(_.isDefined).map(_.get).zipWithIndex.filter(vi => vi._2 % takeEvery == 0).map { vi =>
      Seq(vi._1.x, vi._1.y)}.flatten.toArray
  }
}

object MarkerWithGapsAsMCFMarker {
  def apply(m: MarkerWithGaps, takeEvery: Int = 1): MarkerWithGapsAsMCFMarker =
    new MarkerWithGapsAsMCFMarker(m, takeEvery)
}
