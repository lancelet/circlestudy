package pwa

import java.io.{FileWriter, IOException, File}

import pwa.Geom.{Vec2D, Circle}

import scala.collection.immutable._
import scala.math.Pi

import c3d.{Point, Vec3D, C3D}
import core._

import scalaz._, Scalaz._

import PlateUtils.RichC3D

import PointMassCheck._


object ExportForcesInCircleCoords extends App {

  val forceThreshold: Float = 150.0f // N
  val minContactDuration: Float = 0.1f // s
  val extendByNSamples: Int = 20 // number of samples

  val baseOutDir: File = new File("/Users/jsm/Documents/dev/circlestudy/output")
  val tsOutDirDisj: Throwable \/ File = OutputManager(baseOutDir)
    .createTimeStampOutputDir
    .leftMap(new IOException(_))

  val dataStore: DataStore = new DataStore
  val horses: List[Horse] = dataStore.horses()
  val footfallsDisj: Throwable \/ Seq[(Horse,Footfall)] =
    horses
      .right[Throwable]  // lift to disjunction monad
      .flatMap(
        _.map(
          (h: Horse) =>
            MarkerSet
              .footfalls(dataStore, h, forceThreshold, minContactDuration)
              .map(fs => fs.map((h, _)))
        )
        .sequenceU
      )
      .map(_.flatten)

  val resultStraight: Throwable \/ Unit = for {
    tsOutDir   <- tsOutDirDisj
    hfootfalls <- footfallsDisj
  } yield {
    for {
      (horse, footfall) <- hfootfalls
      if !footfall.direction.isCircle
    } {
      val fw = new FileWriter(new File(tsOutDir, fileName(horse, footfall)))

      // Header
      fw.csvLine(s"C3D source file: ${footfall.c3d.source}")
      fw.csvLine("Horse:", horse.id.toString,
        "Gait:", footfall.gait.toString,
        "Direction:", footfall.direction.toString,
        "Trial Number:", trialNumber(footfall.c3d).toString,
        "Limb:", footfall.limb.toString,
        "Plate:", footfall.plateNumber.toString,
        "Sample Freq (Hz):", "100",
        "Foot on (sample number):", footfall.interval.on.toString,
        "Foot off (sample number):", footfall.interval.off.toString)
      fw.csvLine("", "|--", "World (N)", "--|", "|--", "Cylindrical (N)", "--|")
      fw.csvLine("Frame", "X", "Y", "Z")

      // Force measurements
      val c3d = footfall.c3d
      val plate = c3d.platforms.plates(footfall.plateNumber - 1)  // NOTE: zero-based index
      val rawForces: IndexedSeq[Vec3D] = plate
          .force
          .resampleByAveraging(c3d.fpToPtFactor)

      // go 20 frames before the start, and 20 frames after the end (limited to the trial range)
      val fromFrame = math.max(0,
        footfall.interval.on  - extendByNSamples)
      val toFrame   = math.min(c3d.points.totalSamples - 1,
        footfall.interval.off + extendByNSamples)

      for {
        frame   <- fromFrame until toFrame
        fRaw    =  rawForces(frame)
      } {
        fw.csvLine(frame.toString,
          fRaw.x.toString, fRaw.y.toString, fRaw.z.toString)
      }

      fw.close
    }
  }

  val resultCircle: Throwable \/ Unit = for {
    tsOutDir   <- tsOutDirDisj
    hfootfalls <- footfallsDisj
  } yield {
    for {
      (horse, footfall) <- hfootfalls
      if footfall.direction.isCircle
      c3d               = footfall.c3d
      com               =  memoizedCOM(horse, footfall)
      comCircleDisj     =  \/.fromTryCatch(memoizedCOMCircle(com))
      // go 20 frames before the start, and 20 frames after the end (limited to the trial range)
      fromFrame = math.max(0, footfall.interval.on  - extendByNSamples)
      toFrame   = math.min(c3d.points.totalSamples - 1, footfall.interval.off + extendByNSamples)
      if comIsDefinedFor(com, fromFrame, toFrame)
      if comCircleDisj.isRight
    } {
      val comCircle = comCircleDisj.getOrElse(throw new Exception("comCircleDisj should be defined"))
      val fw = new FileWriter(new File(tsOutDir, fileName(horse, footfall)))

      // Header
      fw.csvLine(s"C3D source file: ${footfall.c3d.source}")
      fw.csvLine("Horse:", horse.id.toString,
        "Gait:", footfall.gait.toString,
        "Direction:", footfall.direction.toString,
        "Trial Number:", trialNumber(footfall.c3d).toString,
        "Limb:", footfall.limb.toString,
        "Plate:", footfall.plateNumber.toString,
        "Circle Center x (mm):", comCircle.origin.x.toString,
        "Circle Center y (mm):", comCircle.origin.y.toString,
        "Circle Radius (mm):", comCircle.radius.toString,
        "Sample Freq (Hz):", "100",
        "Foot on (sample number):", footfall.interval.on.toString,
        "Foot off (sample number):", footfall.interval.off.toString)
      fw.csvLine("", "|--", "World (N)", "--|", "|--", "Cylindrical (N)", "--|")
      fw.csvLine("Frame", "X", "Y", "Z", "Radial", "Tangential", "Vertical", "Angle (rad)")

      // Force measurements
      val plate = c3d.platforms.plates(footfall.plateNumber - 1)  // NOTE: zero-based index
      val rawForces: IndexedSeq[Vec3D] = plate
        .force
        .resampleByAveraging(c3d.fpToPtFactor)

      for {
        frame   <- fromFrame until toFrame
        fRaw    =  rawForces(frame)
        comVec  =  com(frame).get
        fCircle =  forceToCircle(footfall, comCircle)(comVec, fRaw)
      } {
        fw.csvLine(frame.toString,
          fRaw.x.toString, fRaw.y.toString, fRaw.z.toString,
          fCircle.radial.toString, fCircle.tangential.toString, fCircle.vertical.toString,
          fCircle.angle.toString)
      }

      fw.close
    }
  }

  resultCircle.valueOr(throw _)

  def fileName(horse: Horse, footfall: Footfall): String = {
    val hId   = horse.id
    val gait  = footfall.gait.toString
    val dirn  = footfall.direction.toString
    val trial = trialNumber(footfall.c3d)
    val limb  = footfall.limb.toString
    val plate = footfall.plateNumber
    s"forces-horse$hId-$gait-$dirn-$limb-$plate-$trial.csv"
  }

  def trialNumber(c3d: C3D): Int = {
    val regex = """.*?([0-9]+)\.c3d""".r
    c3d.source match {
      case regex(number) => number.toInt
      case _             => throw new Exception("Could not find number in file name")
    }
  }

  implicit class RichWriter(w: FileWriter) {
    def csvLine(items: String*): Unit = w.write(s"${items.mkString(",")}\n")
  }

  final case class CircleForce(radial: Float, tangential: Float, vertical: Float, angle: Float)

  def forceToCircle(footfall: Footfall, comCircle: Circle)(pt: Vec3D, f: Vec3D): CircleForce = {
    // rotation angle between radial vector and forward vector
    val tanUnitAngle = footfall.direction match {
      case Direction.CircleLeft  =>  (Pi / 2.0).toFloat
      case Direction.CircleRight => -(Pi / 2.0).toFloat
      case _                     => throw new Exception("direction must be a circle")
    }
    // angle of point on circle
    val angle = (Vec2D.fromVec3Dxy(pt) - comCircle.origin).angle
    // unit vectors
    val radUnit = Vec2D.fromPolar(1.0f, angle).asVec3D
    val tanUnit = Vec2D.fromPolar(1.0f, angle + tanUnitAngle).asVec3D
    val verUnit = Vec3D(0, 0, 1)
    assert((radUnit dot tanUnit) < 1e-5f)
    assert((radUnit dot verUnit) < 1e-5f)
    assert((tanUnit dot verUnit) < 1e-5f)
    // dot the force vector against the unit vectors to find the force components
    CircleForce(radial = f dot radUnit, tangential = f dot tanUnit, vertical = f dot verUnit,
                angle = angle)
  }

  def comIsDefinedFor(com: Point, fromSample: Int, toSample: Int): Boolean =
    (fromSample until toSample).forall(com(_).isDefined)

  /*
  def comIsDefinedForFootfall(f: Footfall, com: Point): Boolean =
    (f.interval.on until f.interval.off).forall(com(_).isDefined)
  */

}
