package pwa

import scala.math._
import scala.collection.immutable._

import java.io.{File, FileWriter, Writer}

import c3d._
import c3d.util.transform.{RotationMatrix, XForm}

import Geom._
import core._
import core.DataStore.{ RichC3D => DataStoreRichC3D }
import core.PlateUtils.{ RichC3D => HoofUtilsRichC3D }
import core.MarkerSet.{ RichC3D => MarkerSetRichC3D }
import pwa.Geom.Circle

object PWA {
  
  val OnlyOneHorse: Boolean = false /* flag to indicate only use one horse */ 

  val outDir: File = new File("/Users/jsm/Documents/dev/circlestudy/output/pwa")
  val dataDir: File = new File("/Users/jsm/Documents/dev/circlestudy/data")
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
      val trialFootfalls = footfallsInTrial(static, motionTrial)
      val hoofPoints = c3do.hoofPointsForAllLimbs(static)
      val segmentCOMs: Seq[Point] = Buchner.MB.bodies.map(_.point(static, c3do)).map(new MemoizedPoint(_))
      val bodyCOM: Point = new MemoizedPoint(Buchner.bodyCOM(static, c3do))
      val outFile = new File(outDir, "Horse3_circle_right_trot_2.m4v")
      Viz.make_movie(c3do, outFile, imgDir, 4000.0f, trialFootfalls, hoofPoints, segmentCOMs, bodyCOM)
    }
    
    // export stride data for Sandra
    if (true) {
      val curHorses: Seq[Horse] = dataStore.horses(if (OnlyOneHorse) Seq(Horse(3)) else Seq.empty)
      val strideDataDir: File = new File("/Users/jsm/Documents/dev/circlestudy/output/stride-timings")
      def csvFileNameFromC3DFileName(c3dFileName: String): String = {
        val f = new File(c3dFileName)
        f.getName().dropRight(3) ++ "csv"
      }
      for {
        horse <- curHorses
        footfallsByC3DFile: Map[String,Seq[Footfall]] = footfalls(horse).groupBy(_.c3d.source)
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

    // export all motion trials for all horses
    if (false) {
      val curHorses: Seq[Horse] = Seq(Horse(9))
      val imgDir = new File(outDir, "renderMotionTrial")
      def outFile(source: String): File = 
        new File(outDir, s"trialMovies/${(new File(source)).getName.dropRight(4)}.m4v")
      for {
        horse: Horse <- curHorses
        static: C3D = dataStore.staticTrial(horse).valueOr { t: Throwable => throw t }
        trial: MotionTrial <- dataStore.motionTrials(horse)
        c3d: C3D = trial.c3d.valueOr { t: Throwable => throw t }
        trialFootfalls = footfallsInTrial(static, trial)
        hoofPoints = c3d.hoofPointsForAllLimbs(static)
        segmentCOMs: Seq[Point] = Buchner.MB.bodies.map(_.point(static, c3d)).map(new MemoizedPoint(_))
        bodyCOM: Point = new MemoizedPoint(Buchner.bodyCOM(static, c3d))
      } Viz.make_movie(c3d, outFile(c3d.source), imgDir, 4000.0f, trialFootfalls, hoofPoints, segmentCOMs, bodyCOM)
    }

    if (false) {
      import Direction._
      
      // file for a table of footfalls
      val pwaTable: Writer = new FileWriter(new File("/Users/jsm/Documents/dev/circlestudy/output/pwa/pwaTable.csv"))
      pwaTable.write("File Name,Horse ID,Direction,isCircle,Gait,Limb,isForelimb,Plate Number,")
      pwaTable.write("PWA x,PWA y,Out,Radius,Speed,isOuterLimb,F Peak\n")
      pwaTable.flush()
      
      // iterate over all horses, writing footfall data
      for {
        horse <- dataStore.horses(if (OnlyOneHorse) Seq(Horse(3)) else Seq.empty)
        footfall <- footfalls(horse)
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
        val radius: Float = if (direction.isCircle) t6Circle(footfall.c3d).radius else -1.0f
        val speed: Float = trialT6Speed(footfall.c3d, direction) / 1000.0f // m/s
        val fPeak: Float = peakForce(footfall)
        pwaTable.write(s"$fileName,$horseId,$direction,$isCircle,$gait,$limb,$isForelimb,$plateNumber,$x,$y,")
        pwaTable.write(s"$inoutPos,$radius,$speed,$outerLimb,$fPeak\n")
        pwaTable.flush()
      }

      // close the table of footfalls
      pwaTable.close()
    }
  }

  def footfalls(horse: Horse): Seq[Footfall] = {
    val static: C3D = dataStore.staticTrial(horse).valueOr { t: Throwable => throw t }
    val mt: Seq[MotionTrial] = dataStore.motionTrials(horse)
    (for {
      m <- mt
    } yield {
      val c3d: C3D = m.c3d.valueOr { t: Throwable => throw t }
      println(s"Finding footfalls for trial '${c3d.source}'")
      footfallsInTrial(static, m)
    }).flatten
  }

  def footfallsInTrial(static: C3D, motion: MotionTrial): Seq[Footfall] = {
    val c3d = motion.c3d.valueOr { t: Throwable => throw t }
    case class FileFootfall(direction: Direction, gait: Gait, plateNumber: Int, 
            forceWeightedPWA: Vec2D, limb: Limb, interval: ContactInterval, c3d: C3D) extends Footfall
    for {
      contactInterval <- c3d.contactIntervals(forceThreshold, minContactDuration)
      limbOpt = c3d.limbForContactInterval(static, contactInterval)
      if (limbOpt.isDefined)
    } yield {
      val limb = limbOpt.get
      val pwa = forceWeightedPWA(static, c3d, contactInterval, limb)
      val plateNumber: Int = c3d.platforms.plates.indexOf(contactInterval.plate) + 1
      FileFootfall(motion.direction, motion.gait, plateNumber, pwa, limb, contactInterval, c3d)
    }
  }

  def forceWeightedPWA(static: C3D, c3d: C3D, interval: ContactInterval, limb: Limb): Vec2D = {
    val worldToHoof: XForm = worldToHoofTransform(static, c3d, interval, limb)
    val weightedPWA: Vec3D = pwaWeightedDuringContact(c3d, interval)
    val hoofPWA: Vec3D = worldToHoof(weightedPWA)
    Vec2D(hoofPWA.x, hoofPWA.y)
  }
  
  def worldToHoofTransform(static: C3D, c3d: C3D, interval: ContactInterval, limb: Limb): XForm = {
    val hoofPoints: Seq[Point] = c3d.hoofPointsForLimb(limb, static)
    val medPoint: Point = hoofPoints.find(_.name.contains("HoofMedquarter")).get
    val latPoint: Point = hoofPoints.find(_.name.contains("HoofLatquarter")).get
    val dorPoint: Point = hoofPoints.find(_.name.contains("HoofToe")).get
    val med: Vec3D = averagePt(medPoint, interval.percent20, interval.percent80)
    val lat: Vec3D = averagePt(latPoint, interval.percent20, interval.percent80)
    val dor: Vec3D = averagePt(dorPoint, interval.percent20, interval.percent80)
    val origin: Vec3D = {
      val o = (med + lat) / 2.0f
      Vec3D(o.x, o.y, 0)
    }
    val yvec: Vec3D = {
      val y = (dor - origin).asUnit
      Vec3D(y.x, y.y, 0)
    }
    val angle: Double = -90.0 * math.Pi / 180.0
    val xvec: Vec3D = {
      val xx: Float = (yvec.x * cos(angle) + yvec.y * sin(-angle)).toFloat
      val xy: Float = (yvec.x * sin(angle) + yvec.y * cos(angle)).toFloat
      Vec3D(xx, xy, 0)
    }
    val rotMatrix: RotationMatrix = RotationMatrix.fromBasisVectorsXY(xvec, yvec)
    new XForm {
      def apply(v: Vec3D): Vec3D = rotMatrix(v - origin)
    }
  }
  
  def pwaWeightedDuringContact(c3d: C3D, interval: ContactInterval): Vec3D = {
    val pointRate: Float = c3d.points.rate
    val analogRate: Float = c3d.platforms.rate
    val rateFactor: Int = (analogRate / pointRate).toInt
    val startForceIndex: Int = rateFactor * interval.on
    val endForceIndex: Int   = rateFactor * interval.off
    val mag: IndexedSeq[Float] = interval.plate.force.slice(startForceIndex, endForceIndex).map(_.mag)
    val pwa: IndexedSeq[Vec3D] = interval.plate.pwa.slice(startForceIndex, endForceIndex)
    assert(mag.length == pwa.length)
    var x: Double = 0.0
    var y: Double = 0.0
    var z: Double = 0.0
    var weightSum: Double = 0.0
    var i: Int = 0
    while (i < mag.length) {
      val m: Float = mag(i)
      val v: Vec3D = pwa(i)
      if (!v.x.isNaN && !v.y.isNaN && !v.z.isNaN) {  // why would these ever be NaN?  some issue with PWA routine
        x += v.x * m
        y += v.y * m
        z += v.z * m
        weightSum += m
      }
      i += 1
    }
    assert(weightSum > 0.0)
    Vec3D((x / weightSum).toFloat, (y / weightSum).toFloat, (z / weightSum).toFloat)
  }

  def t6Circle(c3d: C3D): Circle = {
    val t6: Point = c3d.getCSPoint("T6").get
    val firstIndex: Int = t6.indexWhere(_.isDefined)
    val lastIndex:  Int = t6.lastIndexWhere(_.isDefined)
    val midIndex:   Int = t6.indexWhere(_.isDefined, (firstIndex + lastIndex) / 2)
    val p1: Vec3D = t6(firstIndex).get
    val p2: Vec3D = t6(midIndex).get
    val p3: Vec3D = t6(lastIndex).get
    circleThrough3Points(Vec2D(p1.x, p1.y), Vec2D(p2.x, p2.x), Vec2D(p3.x, p3.y))
  }
        
  def trialT6Speed(c3d: C3D, direction: Direction): Float = 
    if (direction.isCircle) circleTrialT6Speed(c3d) else straightTrialT6Speed(c3d)
  
  def straightTrialT6Speed(c3d: C3D): Float = {
    val t6: Point = c3d.getCSPoint("T6").get
    val firstIndex: Int = t6.indexWhere(_.isDefined)
    val lastIndex:  Int = t6.lastIndexWhere(_.isDefined)
    val deltaT: Float = (lastIndex - firstIndex) / c3d.points.rate
    val deltaX: Float = (t6(lastIndex).get - t6(firstIndex).get).mag
    deltaX / deltaT
  }
  
  def circleTrialT6Speed(c3d: C3D): Float = {
    val circle: Circle = t6Circle(c3d)
    val t6: Point = c3d.getCSPoint("T6").get
    val firstIndex: Int = t6.indexWhere(_.isDefined)
    val lastIndex:  Int = t6.lastIndexWhere(_.isDefined)
    val p0: Vec3D = t6(firstIndex).get
    val p1: Vec3D = t6(lastIndex).get
    val a0: Double = atan2(p0.y - circle.origin.y, p0.x - circle.origin.x)
    val a1: Double = atan2(p1.y - circle.origin.y, p1.x - circle.origin.x)
    val da: Float = (a1 - a0).toFloat
    val deltaT: Float = (lastIndex - firstIndex) / c3d.points.rate
    val deltaX: Float = abs(da * circle.radius)
    deltaX / deltaT
  }

  def peakForce(footfall: Footfall): Float = {
    val plate: ForcePlate = footfall.interval.plate
    val fpRate: Float = plate.rate
    val ptRate: Float = footfall.c3d.points.rate
    val rateFactor: Int = (fpRate / ptRate).toInt
    val on: Int = footfall.interval.on * rateFactor
    val off: Int = footfall.interval.off * rateFactor
    val fMax: Float = plate.force.iterator.slice(on, off).map(_.mag).max
    fMax
  }

}
