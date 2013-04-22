package circlestudy.optim

import scala.collection.immutable._
import circlestudy.trials.Trial.MarkerWithGaps
import circlestudy.{Vec2, Vec3}

/** Represents a Trial.MarkerWithGaps as a MultiCircleFit.Marker. */
final class MarkerWithGapsAsMCFMarker(m: MarkerWithGaps) extends MultiCircleFit.Marker {
  lazy val co2XY: Array[Double] = m.co.withFilter(_.isDefined).map(_.get).map(v => Seq(v.x, v.y)).flatten.toArray
}

object MarkerWithGapsAsMCFMarker {
  def apply(m: MarkerWithGaps): MarkerWithGapsAsMCFMarker = new MarkerWithGapsAsMCFMarker(m)
}
