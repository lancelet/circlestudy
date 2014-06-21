package pwa

import scala.collection.immutable._

import java.io.{IOException, File, FileWriter, Writer}

import c3d._

import Geom._
import core._
import core.MarkerSet.{ RichC3D => MarkerSetRichC3D }
import core.MarkerSet.{ footfalls, footfallsInTrial, peakForce }

object PWA {
  
  val OnlyOneHorse: Boolean = true /* flag to indicate only use one horse */

  val dataDir: File = new File("/Users/jsm/Documents/dev/circlestudy/data")

  val baseOutDir: File = new File("/Users/jsm/Documents/dev/circlestudy/output")
  val tsOutDir: File = OutputManager(baseOutDir).createTimeStampOutputDir.valueOr { s: String =>
    throw new IOException(s)
  }
  val outDir: File = new File(tsOutDir, "pwa")

  val forceThreshold: Float = 150.0f // N
  val minContactDuration: Float = 0.1f // s

  val dataStore = new DataStore(dataDir)

  def main(args: Array[String]) = {

    // test graphics routines
    if (false) {
      // test graphics routines
      val c3do = C3D.read(new File(dataDir, "Horse 3/Horse 3 hard surface circle/Horse3_circle_right_trot_2.c3d"))
      val imgDir = new File(outDir, "renderTest")
      val static: C3D = dataStore.staticTrial(Horse(3)).valueOr { t: Throwable => throw t }
      val motionTrial: MotionTrial = new MotionTrial {
        def direction: Direction = Direction.CircleLeft
        def gait: Gait = Gait.Trot
        def c3d: C3D = c3do
      }
      val trialFootfalls = footfallsInTrial(static, motionTrial, forceThreshold, minContactDuration)
      val hoofPoints = c3do.hoofPointsForAllLimbs(static)
      val segmentCOMs: Seq[Point] = Buchner.MB.bodies.map(_.point(static, c3do)).map(new MemoizedPoint(_))
      val bodyCOM: Point = new MemoizedPoint(Buchner.bodyCOM(static, c3do))
      val outFile = new File(outDir, "Horse3_circle_right_trot_2.m4v")
      Viz.make_movie(c3do, outFile, imgDir, 4000.0f, trialFootfalls, hoofPoints, segmentCOMs, bodyCOM)
    }
    
    // export stride data for Sandra
    if (false) {
      val curHorses: Seq[Horse] = dataStore.horses(if (OnlyOneHorse) Seq(Horse(3)) else Seq.empty)
      val strideDataDir: File = new File(tsOutDir, "stride-timings")
      strideDataDir.mkdirs()
      def csvFileNameFromC3DFileName(c3dFileName: String): String = {
        val f = new File(c3dFileName)
        f.getName().dropRight(3) ++ "csv"
      }
      println("Exporting stride data for Sandra")
      for {
        horse <- curHorses
        footfallsByC3DFile: Map[String,Seq[Footfall]] = footfalls(
            dataStore, horse, forceThreshold, minContactDuration
          ).valueOr(throw _).groupBy(_.c3d.source)
        c3dFileName <- footfallsByC3DFile.keys
        csvFileName = csvFileNameFromC3DFileName(c3dFileName)
        footfallsForCurrentC3DFile: Seq[Footfall] = footfallsByC3DFile(c3dFileName)
      } {
        println(s"Processing file $c3dFileName -> $csvFileName")
        val w: Writer = new FileWriter(new File(strideDataDir, csvFileName))
        w.write(s"Source CSV file:,$c3dFileName\n")
        w.write(s"Force threshold (N):,$forceThreshold\n")
        w.write("Limb,Stance start (sample number),Stance end (sample number)\n")
        for {
          footfall <- footfallsForCurrentC3DFile
          limb = footfall.limb
          start = footfall.interval.on
          end = footfall.interval.off
        } {
          w.write(s"$limb,$start,$end\n")
        }
        w.close()
      }
    }

    // export movies of all motion trials for all horses (needs ffmpeg to be installed)
    if (false) {
      println("Exporting movies of all motion trials for all horses")
      val curHorses: Seq[Horse] = dataStore.horses(if (OnlyOneHorse) Seq(Horse(3)) else Seq.empty)
      val imgDir = new File(tsOutDir, "renderMotionTrial"); imgDir.mkdirs()
      val tmDir = new File(tsOutDir, "trialMovies"); tmDir.mkdirs()
      def outFile(source: String): File =
        new File(tmDir, s"${new File(source).getName.dropRight(4)}.m4v")
      for {
        horse: Horse <- curHorses
        static: C3D = dataStore.staticTrial(horse).valueOr { t: Throwable => throw t }
        trial: MotionTrial <- dataStore.motionTrials(horse).valueOr { t: Throwable => throw t }
        c3d: C3D = trial.c3d
        trialFootfalls = footfallsInTrial(static, trial, forceThreshold, minContactDuration)
        hoofPoints = c3d.hoofPointsForAllLimbs(static)
        segmentCOMs: Seq[Point] = Buchner.MB.bodies.map(_.point(static, c3d)).map(new MemoizedPoint(_))
        bodyCOM: Point = new MemoizedPoint(Buchner.bodyCOM(static, c3d))
      } Viz.make_movie(c3d, outFile(c3d.source), imgDir, 4000.0f, trialFootfalls, hoofPoints, segmentCOMs, bodyCOM)
    }

    if (true) {
      import Direction._

      println("Exporting table of PWA footfalls")

      // file for a table of footfalls
      val pwaTable: Writer = new FileWriter(new File(tsOutDir, "pwaTable.csv"))
      pwaTable.write("File Name,Horse ID,Direction,isCircle,Gait,Limb,isForelimb,Plate Number,")
      pwaTable.write("PWA x,PWA y,Out,Radius,Speed,isOuterLimb,F Peak\n")
      pwaTable.flush()
      
      // iterate over all horses, writing footfall data
      for {
        horse <- dataStore.horses(if (OnlyOneHorse) Seq(Horse(3)) else Seq.empty)
        footfall <- footfalls(dataStore, horse, forceThreshold, minContactDuration).valueOr(throw _)
        fileName = footfall.c3d.source
        horseId = horse.id
        direction = footfall.direction
        gait = footfall.gait
        limb = footfall.limb
        plateNumber = footfall.plateNumber
        x = footfall.forceWeightedPWA.x
        y = footfall.forceWeightedPWA.y
        outerLimb = if (footfall.isOuterLimb) "true" else "false"
        isCircle = if(footfall.direction.isCircle) "true" else "false"
        isForelimb = if (footfall.limb.isForelimb) "true" else "false"
      } {
        val inoutPos: Float = if (direction == CircleLeft) x else if (direction == CircleRight) -x else x
        val radius: Float = if (direction.isCircle) footfall.c3d.t6CircleThrough3Points.radius else -1.0f
        val speed: Float = footfall.c3d.t6AverageSpeed(direction) / 1000.0f // m/s
        val fPeak: Float = peakForce(footfall)
        pwaTable.write(s"$fileName,$horseId,$direction,$isCircle,$gait,$limb,$isForelimb,$plateNumber,$x,$y,")
        pwaTable.write(s"$inoutPos,$radius,$speed,$outerLimb,$fPeak\n")
        pwaTable.flush()
      }

      // close the table of footfalls
      pwaTable.close()
    }
  }

}
