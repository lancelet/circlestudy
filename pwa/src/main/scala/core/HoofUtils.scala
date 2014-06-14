package core

import pwa.Geom.Vec2D
import c3d.{ForcePlate, C3D}

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

/** A single contact interval of a force plate. */
case class ContactInterval (on: Int, off: Int, plate: ForcePlate) {
  def range: Int = off - on
  def fraction(f: Float): Int = on + (range * f).toInt
  lazy val percent20: Int = fraction(0.2f)
  lazy val percent80: Int = fraction(0.8f)
}


object HoofUtils {

}
