package pwa

import scala.math._
import scala.collection.breakOut
import scala.collection.immutable._
import scala.collection.mutable.Buffer
import java.io.File
import java.io.FilenameFilter
import java.io.FileWriter
import java.io.Writer
import c3d._
import c3d.util.transform.VirtualPoint
import c3d.util.transform.RotationMatrix
import c3d.util.transform.XForm
import Geom._

object PWA {
  
  val OnlyOneHorse: Boolean = false /* flag to indicate only use one horse */ 

  def main(args: Array[String]) = {

    // test graphics routines
    if (false) {
      // test graphics routines
      val c3do = C3D.read(new File(dataDir, "Horse 3/Horse 3 hard surface circle/Horse3_circle_right_trot_2.c3d"))
      val imgDir = new File(outDir, "renderTest")
      val static: C3D = staticC3D(Horse(3))
      val motionTrial: MotionTrial = new MotionTrial {
        def direction: Direction = Direction.CircleLeft
        def gait: Gait = Gait.Trot
        def c3d: C3D = c3do
      }
      val trialFootfalls = footfallsInTrial(static, motionTrial)
      val hoofPoints = (hoofPointsForLimb(static, c3do, Limb.LF) ++
        hoofPointsForLimb(static, c3do, Limb.RF) ++
        hoofPointsForLimb(static, c3do, Limb.LH) ++
        hoofPointsForLimb(static, c3do, Limb.RH))
      val segmentCOMs: Seq[Point] = Buchner.MB.bodies.map(_.point(static, c3do)).map(new MemoizedPoint(_))
      val bodyCOM: Point = new MemoizedPoint(Buchner.bodyCOM(static, c3do))
      val outFile = new File(outDir, "Horse3_circle_right_trot_2.m4v")
      Viz.make_movie(c3do, outFile, imgDir, 4000.0f, trialFootfalls, hoofPoints, segmentCOMs, bodyCOM)
    }
    
    // export all motion trials for all horses
    if (true) {
      val curHorses: Seq[Horse] = Seq(Horse(9))
      val imgDir = new File(outDir, "renderMotionTrial")
      def outFile(source: String): File = 
        new File(outDir, s"trialMovies/${(new File(source)).getName.dropRight(4)}.m4v")
      for {
        horse: Horse <- curHorses
        static: C3D = staticC3D(horse)
        trial: MotionTrial <- motionTrials(horse)
        c3d: C3D = trial.c3d
        trialFootfalls = footfallsInTrial(static, trial)
        hoofPoints = (hoofPointsForLimb(static, c3d, Limb.LF) ++
          hoofPointsForLimb(static, c3d, Limb.RF) ++
          hoofPointsForLimb(static, c3d, Limb.LH) ++
          hoofPointsForLimb(static, c3d, Limb.RH))
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
        horse <- horses
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
  
  val outDir: File = new File("/Users/jsm/Documents/dev/circlestudy/output/pwa")
  val dataDir: File = new File("/Users/jsm/Documents/dev/circlestudy/data")
  val forceThreshold: Float = 150.0f // N
  val minContactDuration: Float = 0.1f // s

  sealed trait Direction {
    import Direction._
    def isCircle: Boolean = (this == CircleLeft) || (this == CircleRight)
  }
  object Direction {
    object Straight    extends Direction { override def toString = "straight"     }
    object CircleRight extends Direction { override def toString = "circle right" }
    object CircleLeft  extends Direction { override def toString = "circle left"  }    
  }
  
  sealed trait Gait
  object Gait {
    object Walk extends Gait { override def toString = "walk" }
    object Trot extends Gait { override def toString = "trot" }    
  }
  
  sealed trait Limb {
    import Limb._
    def isForelimb: Boolean = (this == LF) || (this == RF)
  }
  object Limb {
    object LF extends Limb { override def toString = "LF" }
    object RF extends Limb { override def toString = "RF" }
    object LH extends Limb { override def toString = "LH" }
    object RH extends Limb { override def toString = "RH" }
  }
  
  final case class Horse (id: Int)
  def horses: Seq[Horse] = {
    if (OnlyOneHorse) {
      Seq(Horse(10))
    } else {
      val subdirs: Seq[File] = dataDir.listFiles.to[Seq].filter(_.isDirectory)
      val horseNumberStrings: Seq[String] = subdirs.map(_.getName).filter(_.startsWith("Horse")).map(_.drop(6))
      horseNumberStrings.map((s: String) => Horse(s.toInt)).sortBy(_.id)
    }
  }
  
  def getPoint(c3d: C3D, name: String): Point = {
    // mapping of unusual names encountered in the trials (spelling mistakes)
    val oddNameMapping: Map[String, String] = Map (
      ("RFHoofLatTopfffff", "RFHoofLatTop"),
      ("RHHeeld", "RHHeel")
    )
    val actualName: String = oddNameMapping.getOrElse(name.trim, name.trim)
    c3d.points.getPointByName(actualName).getOrElse(  // first try fetching by a normal name
      c3d.points.getPointByDescription(actualName).getOrElse(  // then try fetching by a description
        c3d.points.points.find(_.name.contains(actualName)).getOrElse(  // then partial name
          c3d.points.points.find(_.description.contains(actualName)).getOrElse {  // then partial description
            val err = s"could not find point $actualName in C3D file ${c3d.source}"
            throw new NoSuchElementException(err)
          }
        )
      )
    )
  }
  
  def footfalls(horse: Horse): Seq[Footfall] = {
    val static: C3D = staticC3D(horse)
    val mt: Seq[MotionTrial] = motionTrials(horse)
    (for {
      m <- mt
    } yield {
      println(s"Finding footfalls for trial '${m.c3d.source}'")
      footfallsInTrial(static, m)
    }).flatten
  }
  
  def staticC3D(horse: Horse): C3D = {
    def fmatcher(name: String): Boolean = name.startsWith(s"Horse${horse.id}_") && name.contains("static_virtual")
    val files: Seq[File] = recursiveFileSearch(dataDir, fmatcher)
    assert(files.length == 1, s"${files.length} static virtual files found for horse ${horse.id}")
    C3D.read(files.head)
  }

  def motionTrials(horse: Horse): Seq[MotionTrial] = {
    def fmatcher(name: String): Boolean = (
      name.startsWith(s"Horse${horse.id}_") && (name.contains("walk") || name.contains("trot")) &&
      name.endsWith(".c3d")
    )
    val files: Seq[File] = recursiveFileSearch(dataDir, fmatcher)
    case class FileMotionTrial(file: File) extends MotionTrial {
      import Direction._
      import Gait._
      private val name: String = file.getName
      def has(subString: String): Boolean = name.contains(subString)
      val direction: Direction = if (has("left")) CircleLeft else if (has("right")) CircleRight else Straight
      val gait: Gait = if (has("walk")) Walk else Trot
      def c3d: C3D = try {
        C3D.read(file)
      } catch {
        case e: Exception => {
          println(s"problem reading file ${file.getName}")
          throw e
        }
      }
    }
    files.map(FileMotionTrial(_))
  }
  
  def footfallsInTrial(static: C3D, motion: MotionTrial): Seq[Footfall] = {
    val c3d = motion.c3d
    case class FileFootfall(direction: Direction, gait: Gait, plateNumber: Int, 
            forceWeightedPWA: Vec2D, limb: Limb, interval: ContactInterval, c3d: C3D) extends Footfall
    for {
      contactInterval <- contactIntervals(c3d)
      limbOpt = limbForContactInterval(static, c3d, contactInterval)
      if (limbOpt.isDefined)
    } yield {
      val limb = limbOpt.get
      val pwa = forceWeightedPWA(static, c3d, contactInterval, limb)
      val plateNumber: Int = c3d.platforms.plates.indexOf(contactInterval.plate) + 1
      FileFootfall(motion.direction, motion.gait, plateNumber, pwa, limb, contactInterval, c3d)
    }
  }
  
  def contactIntervals(c3d: C3D): Seq[ContactInterval] = {
    val fpRate: Float = c3d.platforms.rate
    val ptRate: Float = c3d.points.rate
    val resampleFactor: Int = (fpRate / ptRate).toInt
    val ciByPlate: Seq[Seq[ContactInterval]] = for {
      plate <- c3d.platforms.plates
      forceMag = plate.force.resampleByAveraging(resampleFactor).map(_.mag)
    } yield {
      def accum(accumCI: Seq[ContactInterval], startIndex: Int): Seq[ContactInterval] = {
        val on: Int  = forceMag.indexWhere(_ >= forceThreshold, startIndex)
        val off: Int = forceMag.indexWhere(_ < forceThreshold, on)
        if (on == -1 || off == -1) {
          accumCI.reverse
        } else {
          accum(ContactInterval(on, off, plate) +: accumCI, off)
        }
      }
      accum(Seq.empty[ContactInterval], 0)
    }
    val minDurationInSamples: Int = (minContactDuration * ptRate).toInt
    def exceedsMinDuration(c: ContactInterval): Boolean = (c.off - c.on) >= minDurationInSamples
    ciByPlate.flatten.filter(exceedsMinDuration(_))
  }
  
  def limbForContactInterval(static: C3D, c3d: C3D, interval: ContactInterval): Option[Limb] = {
    // find closest limb
    val closest: Limb = closestLimb(c3d, interval)
    // all markers for closest limb must be within the force plate
    val markers: Seq[Point] = hoofPointsForLimb(static, c3d, closest)
    if (markers.forall(pointWithinPlateForWholeInterval(_, interval))) {
      Some(closest)
    } else {
      None
    }
  }
  
  def forceWeightedPWA(static: C3D, c3d: C3D, interval: ContactInterval, limb: Limb): Vec2D = {
    val worldToHoof: XForm = worldToHoofTransform(static, c3d, interval, limb)
    val weightedPWA: Vec3D = pwaWeightedDuringContact(c3d, interval)
    val hoofPWA: Vec3D = worldToHoof(weightedPWA)
    Vec2D(hoofPWA.x, hoofPWA.y)
  }
  
  def worldToHoofTransform(static: C3D, c3d: C3D, interval: ContactInterval, limb: Limb): XForm = {
    val hoofPoints: Seq[Point] = hoofPointsForLimb(static, c3d, limb)
    val medPoint: Point = hoofPoints.find(_.name.contains("HoofMedquarter")).get
    val latPoint: Point = hoofPoints.find(_.name.contains("HoofLatquarter")).get
    val dorPoint: Point = hoofPoints.find(_.name.contains("HoofToe")).get
    val med: Vec3D = averagePtOverRange(medPoint, interval.percent20, interval.percent80)
    val lat: Vec3D = averagePtOverRange(latPoint, interval.percent20, interval.percent80)
    val dor: Vec3D = averagePtOverRange(dorPoint, interval.percent20, interval.percent80)
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
      
  def pointWithinPlateForWholeInterval(point: Point, interval: ContactInterval): Boolean = {
    val plate = interval.plate
    var curIndex = interval.on
    while (curIndex < interval.off) {
      if (pointWithinPlate(point, curIndex, plate) == false) return false
      curIndex = curIndex + 1
    }
    return true
  }
  
  def pointWithinPlate(point: Point, ptIndex: Int, plate: ForcePlate): Boolean = {
    val optPos: Option[Vec3D] = point(ptIndex)
    if (optPos.isDefined) {
      val pos: Vec3D = optPos.get
      val testPt: Vec2D = Vec2D(pos.x, pos.y)
      val c0: Vec2D = Vec2D(plate.corners(0).x, plate.corners(0).y)
      val c1: Vec2D = Vec2D(plate.corners(1).x, plate.corners(1).y)
      val c2: Vec2D = Vec2D(plate.corners(2).x, plate.corners(2).y)
      val c3: Vec2D = Vec2D(plate.corners(3).x, plate.corners(3).y)
      ptWithinTriangle(c0, c1, c2, testPt) || ptWithinTriangle(c0, c2, c3, testPt)
    } else {
      false
    }
  }
  
  def ptWithinTriangle(cornerA: Vec2D, cornerB: Vec2D, cornerC: Vec2D, testPt: Vec2D): Boolean = {
    val p  = testPt
    val p1 = cornerA
    val p2 = cornerB
    val p3 = cornerC
    val det: Float = (p1.x - p3.x) * (p2.y - p3.y) - (p2.x - p3.x) * (p1.y - p3.y)
    val alpha: Float = ((p2.y - p3.y) * (p.x - p3.x) + (p3.x - p2.x) * (p.y - p3.y)) / det
    val beta: Float  = ((p3.y - p1.y) * (p.x - p3.x) + (p1.x - p3.x) * (p.y - p3.y)) / det
    val gamma: Float = 1.0f - alpha - beta
    (alpha >= 0) && (beta >= 0) && (gamma >= 0)
  }
    
  def closestLimb(c3d: C3D, interval: ContactInterval): Limb = {
    val fpRate: Float = c3d.platforms.rate
    val ptRate: Float = c3d.points.rate
    val sf: Int = (fpRate / ptRate).toInt
    val onpt: Vec3D = interval.plate.pwa(sf * interval.on)
    val toeRF: Point = getPoint(c3d, "RFHoofDorsal")
    val toeLF: Point = getPoint(c3d, "LFHoofDorsal")
    val toeRH: Point = getPoint(c3d, "RHHoofDorsal")
    val toeLH: Point = getPoint(c3d, "LHHoofDorsal")
    val distant: Vec3D = Vec3D(10000f, 10000f, 10000f)
    val dRF: Float = (toeRF(interval.on).getOrElse(distant) - onpt).mag
    val dLF: Float = (toeLF(interval.on).getOrElse(distant) - onpt).mag
    val dRH: Float = (toeRH(interval.on).getOrElse(distant) - onpt).mag
    val dLH: Float = (toeLH(interval.on).getOrElse(distant) - onpt).mag
    import Limb._
    if (dRF <= dLF && dRF <= dRH && dRF <= dLH) {
      RF
    } else if (dLF <= dRH && dLF <= dLH) {
      LF
    } else if (dRH <= dLH) {
      RH
    } else {
      LH
    }
  }
  
  def hoofPointsForLimb(static: C3D, c3d: C3D, limb: Limb): Seq[Point] = {
    val motionPoints: IndexedSeq[Point] = motionHoofPointsForLimb(c3d, limb)
    val virtualPoints: Seq[Point]       = virtualHoofPointsForLimb(static, c3d, limb, motionPoints)
    motionPoints ++ virtualPoints
  }
  
  def motionHoofPointsForLimb(c3d: C3D, limb: Limb): IndexedSeq[Point] = {
    val ls = limb.toString
    val baseNames: IndexedSeq[String] = IndexedSeq("HoofLatBottom", "Heel", "HoofLatTop", "HoofDorsal")
    val names: IndexedSeq[String] = baseNames.map(bn => s"$ls$bn")
    for (name <- names) yield getPoint(c3d, name)
  }
  
  def virtualHoofPointsForLimb(static: C3D, c3d: C3D, limb: Limb, motionPoints: IndexedSeq[Point]): Seq[Point] = {
    val ls = limb.toString
    val baseNames: Seq[String] = Seq("HoofLatHeel", "HoofLatquarter", "HoofToe", "HoofMedquarter", "HoofMedHeel")
    val names: Seq[String] = baseNames.map(bn => s"$ls$bn")
    val staticRefs: IndexedSeq[Vec3D] = for {
      name <- motionPoints.map(_.description)
      staticPt: Point = getPoint(static, name)
    } yield averagePt(staticPt)
    for {
      name <- names
      staticPt: Point = getPoint(static, name)
    } yield (new MemoizedPoint(VirtualPoint(name, "", averagePt(staticPt), staticRefs, motionPoints)))
  }
  
  def averagePtOverRange(p: Point, start: Int, end: Int): Vec3D = {
    var x: Float = 0.0f
    var y: Float = 0.0f
    var z: Float = 0.0f
    var n: Int = 0
    var i: Int = start
    while (i <= end) {
      val vOpt: Option[Vec3D] = p(i)
      if (vOpt.isDefined) {
        val v: Vec3D = vOpt.get
        x += v.x
        y += v.y
        z += v.z
        n += 1
      }
      i += 1
    }
    Vec3D(x / n, y / n, z / n)
  }
  
  def averagePt(p: Point): Vec3D = averagePtOverRange(p, 0, p.length - 1)
  
  def t6Circle(c3d: C3D): Circle = {
    val t6: Point = getPoint(c3d, "T6")
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
    val t6: Point = getPoint(c3d, "T6")
    val firstIndex: Int = t6.indexWhere(_.isDefined)
    val lastIndex:  Int = t6.lastIndexWhere(_.isDefined)
    val deltaT: Float = (lastIndex - firstIndex) / c3d.points.rate
    val deltaX: Float = (t6(lastIndex).get - t6(firstIndex).get).mag
    deltaX / deltaT
  }
  
  def circleTrialT6Speed(c3d: C3D): Float = {
    val circle: Circle = t6Circle(c3d)
    val t6: Point = getPoint(c3d, "T6")
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
  
  def recursiveFileSearch(parentDir: File, fileNameMatcher: String => Boolean): Seq[File] = {
    val filter: FilenameFilter = new FilenameFilter {
      def accept(dir: File, name: String): Boolean = fileNameMatcher(name)
    }
    val curDirFiles: Seq[File] = parentDir.listFiles(filter).to[Seq]
    val curDirDirs: Seq[File] = parentDir.listFiles.filter(_.isDirectory).to[Seq]
    curDirFiles ++ curDirDirs.flatMap(recursiveFileSearch(_, fileNameMatcher))
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
  
  case class ContactInterval (on: Int, off: Int, plate: ForcePlate) {
    def range: Int = off - on
    def fraction(f: Float): Int = on + (range * f).toInt
    lazy val percent20: Int = fraction(0.2f)
    lazy val percent80: Int = fraction(0.8f)
  }
    
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

  trait MotionTrial {
    def direction: Direction
    def gait: Gait
    def c3d: C3D
  }

}
