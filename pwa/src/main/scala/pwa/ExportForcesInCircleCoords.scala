package pwa

import java.io.{FileWriter, IOException, File}

import scala.collection.immutable._

import c3d.{Vec3D, C3D}
import core._

import scalaz._, Scalaz._

import PlateUtils.RichC3D

import PointMassCheck._


object ExportForcesInCircleCoords extends App {

  val forceThreshold: Float = 150.0f // N
  val minContactDuration: Float = 0.1f // s

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

  val result: Throwable \/ Unit = for {
    tsOutDir   <- tsOutDirDisj
    hfootfalls <- footfallsDisj
  } yield {
    for {
      (horse, footfall) <- hfootfalls
    } {
      val fw = new FileWriter(new File(tsOutDir, fileName(horse, footfall)))

      // Header
      fw.csvLine(s"C3D source file: ${footfall.c3d.source}")
      fw.csvLine("Horse:", horse.id.toString,
        "Gait:", footfall.gait.toString,
        "Direction:", footfall.direction.toString,
        "Trial Number:", trialNumber(footfall.c3d).toString,
        "Limb:", footfall.limb.toString,
        "Plate:", footfall.plateNumber.toString)
      fw.csvLine("", "|--", "World (N)", "--|", "|--", "Cylindrical (N)", "--|")
      fw.csvLine("Frame", "X", "Y", "Z", "Radial", "Tangential", "Vertical")

      // Force measurements
      val c3d = footfall.c3d
      val plate = c3d.platforms.plates(footfall.plateNumber - 1)  // NOTE: zero-based index
      val rawForces: IndexedSeq[Vec3D] = plate
        .force
        .resampleByAveraging(c3d.fpToPtFactor)

      for {
        frame <- footfall.interval.on until footfall.interval.off
        fRaw  =  rawForces(frame)
      } {
        fw.csvLine(frame.toString,
                   fRaw.x.toString, fRaw.y.toString, fRaw.z.toString)
      }

      fw.close
    }
  }

  result.valueOr(throw _)

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

}
