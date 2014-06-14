package pwa

import scala.annotation.tailrec
import scala.collection.immutable._
import scala.collection.mutable.{Map => MMap}
import scala.io.Source
import math.{abs, Pi}
import c3d._
import java.io.File
import java.io.FileWriter
import java.io.Writer

import core._

object PointMassCheck {

  import PWA._
  import Geom._
  
  def main(args: Array[String]) = {
    
    val table: Writer = new FileWriter(
        new File("/Users/jsm/Documents/dev/circlestudy/output/pointmasscheck/pmTable.csv"))
    table.write("File Name,Horse ID,Direction,Limb,isOuterLimb,isForeLimb,Tangential,Radial,Vertical,")
    table.write("PredRadial,PredVertical,MaxCircleDevRadiusFraction\n")
    table.flush()
    
    // Tangential   = tangential force multiplied by duty factor of limb divided by horse mass
    // Radial       = radial force multiplied by duty factor of limb divided by horse mass
    // Vertical     = vertical force multiplied by duty factor of limb divided by horse mass
    // PredRadial   = predicted average radial force based upon circular point mass model, divided by horse mass
    // PredVertical = predicted vertical force, divided by horse mass
    // MaxCircleDevRadiusFraction = maximum deviation from fitted circle divided by radius of circle
    
    def horseSet: Seq[Horse] = Seq(Horse(3), Horse(4), Horse(6), Horse(8), Horse(9), Horse(10))
    
    for {
      horse <- horseSet
      footfall <- circleTrotFootfallsWithValidCOM(horse)
      fileName = footfall.c3d.source
      horseId = horse.id
      direction = footfall.direction
      limb = footfall.limb
      isOuterLimb = if (footfall.isOuterLimb) "true" else "false"
      isForelimb = if (footfall.limb.isForelimb) "true" else "false"
      com = memoizedCOM(horse, footfall)
      fc = averageForceComponents(footfall, com)
      df = dutyFactor(horse, limb, direction)
      hmass = mass(horse) 
      tangential = df * fc.tangential / hmass
      radial     = df * fc.radial     / hmass
      vertical   = df * fc.vertical   / hmass
      avgPredRadial   = avgPredictedRadialForce(horse, com) / hmass      
      avgPredVertical = avgPredictedVerticalForce(horse)    / hmass
      maxCircleDev = maxDeviationFromCircleAsRadiusFraction(com)
    } {
      table.write(s"$fileName,$horseId,$direction,$limb,$isOuterLimb,$isForelimb,")
      table.write(s"$tangential,$radial,$vertical,$avgPredRadial,$avgPredVertical,$maxCircleDev\n")
      table.flush()
    }
    
    table.close()
  }

  final case class ForceComponents(tangential: Float, radial: Float, vertical: Float)
  
  def averageForceComponents(footfall: Footfall, com: Point): ForceComponents = {
    val plate: ForcePlate = footfall.interval.plate
    val fpRate: Float = plate.rate
    val ptRate: Float = footfall.c3d.points.rate
    val rateFactor: Int = (fpRate / ptRate).toInt
    @tailrec def findAverage(tangential: Float, radial: Float, vertical: Float, frame3D: Int): ForceComponents = {
      if (frame3D == footfall.interval.off + 1) {
        val n: Float = footfall.interval.off - footfall.interval.on
        ForceComponents(tangential / n, radial / n, vertical / n)
      } else {
        // find angle of current frame, and unit vectors
        val angle: Float = comPointAngle(com, frame3D)
        val radUnit: Vec2D = Vec2D.fromPolar(1.0f, angle)
        val tanUnit: Vec2D = Vec2D.fromPolar(1.0f, (angle + Pi / 2.0).toFloat)
        // dot force vector against radial and tangent vectors
        val f3: Vec3D = plate.force(frame3D * rateFactor)
        val fxy: Vec2D = Vec2D.fromVec3Dxy(f3)
        val fRadial: Float     = fxy dot radUnit
        val fTangential: Float = fxy dot tanUnit
        val fVertical: Float   = f3.z
        // continue summation
        findAverage(tangential + fTangential, radial + fRadial, vertical + fVertical, frame3D + 1)
      }
    }
    findAverage(0.0f, 0.0f, 0.0f, footfall.interval.on)
  }
 
  // Finds the average angular velocity for a COM point
  def angularVelocityForTrial(com: Point): Float = {
    def normAngle(angle: Float): Float = if (angle < 0) (angle + 2.0 * Pi).toFloat else angle
    // find indices, times and angles
    val firstIndex: Int = com.indexWhere(_.isDefined)
    val lastIndex: Int  = com.lastIndexWhere(_.isDefined)
    val firstTime: Float = firstIndex / com.rate
    val lastTime: Float  = lastIndex / com.rate
    val firstAngle: Float = normAngle(comPointAngle(com, firstIndex))
    val lastAngle: Float  = normAngle(comPointAngle(com, lastIndex))
    // find average angular velocity
    val deltaAngle: Float = abs(lastAngle - firstAngle)
    val deltaTime: Float  = lastTime - firstTime
    deltaAngle / deltaTime
  }
  
  // Finds the angle of a COM point at a specified frame
  def comPointAngle(com: Point, frame3D: Int): Float = {
    val circle: Circle = memoizedCOMCircle(com)
    (Vec2D.fromVec3Dxy(com(frame3D).get) - circle.origin).angle
  }
  
  // Get footfalls during which a valid COM is also defined.  (COM may drop out, etc., due to missing markers)
  def circleTrotFootfallsWithValidCOM(horse: Horse): Seq[Footfall] = {
    def comIsDefined(f: Footfall): Boolean = {
      val comPt = memoizedCOM(horse, f)
      (Seq.range(f.interval.on, f.interval.off+1)).forall(comPt(_).isDefined)
    }
    footfalls(horse).filter(_.direction.isCircle).filter(_.gait == Gait.Trot).filter(comIsDefined(_))
  }

  // Memoize COM circles so that they don't need to be re-calculated
  private [this] val comCircleMap: MMap[Point, Circle] = MMap.empty[Point, Circle]
  def memoizedCOMCircle(comPoint: Point): Circle = {
    synchronized {
      if (!comCircleMap.contains(comPoint)) {
        val v2d: IndexedSeq[Vec2D] = comPoint.filter(_.isDefined).map(_.get).map(v => Vec2D(v.x, v.y))
        comCircleMap(comPoint) = lsqCircle(v2d)
      }
      comCircleMap(comPoint)
    }
  }
  
  // Memoize COM points so that they don't need to be re-calculated
  private [this] val comMap: MMap[Footfall, Point] = MMap.empty[Footfall, Point]
  def memoizedCOM(horse: Horse, f: Footfall): Point = {
    synchronized {
      if (!comMap.contains(f)) {
        val static: C3D = dataStore.staticTrial(horse).valueOr { t: Throwable => throw t }
        val comPt: Point = new MemoizedPoint(Buchner.bodyCOM(static, f.c3d))
        comMap(f) = comPt
      }
      comMap(f)
    }
  }
  
  private [this] final case class DutyFactorKey(horse: Horse, limb: Limb, direction: Direction)
  private [this] val dutyFactorMap: Map[DutyFactorKey, Float] = {
    val buildMap: MMap[DutyFactorKey, Float] = MMap.empty[DutyFactorKey, Float]
    val inFile: File = new File("/Users/jsm/Documents/dev/circlestudy/data/avg-duty-factors-per-horse.csv")
    val lines = Source.fromFile(inFile).getLines.drop(2)  // read file, but drop header lines
    for {
      line <- lines
      fields            = line.split(",").map(_.trim)
      limb: Limb        = Limb.fromString(fields(0))
      horse: Horse      = Horse(fields(1).toInt)
      leftDF: Float     = fields(3).toFloat
      rightDF: Float    = fields(4).toFloat
      straightDF: Float = fields(5).toFloat
    } {
      buildMap(DutyFactorKey(horse, limb, Direction.CircleLeft))  = leftDF
      buildMap(DutyFactorKey(horse, limb, Direction.CircleRight)) = rightDF
      buildMap(DutyFactorKey(horse, limb, Direction.Straight))    = straightDF
    }
    Map.empty[DutyFactorKey, Float] ++ buildMap
  }
  def dutyFactor(horse: Horse, limb: Limb, direction: Direction): Float = 
    dutyFactorMap(DutyFactorKey(horse, limb, direction))
  
  // Horse masses in kg
  def mass(horse: Horse): Float = horse match {
    case Horse(1)  => 468.0f
    case Horse(2)  => 435.0f
    case Horse(3)  => 446.0f
    case Horse(4)  => 455.0f
    case Horse(5)  => 478.0f
    case Horse(6)  => 415.0f
    case Horse(7)  => 477.0f
    case Horse(8)  => 456.0f
    case Horse(9)  => 431.0f
    case Horse(10) => 442.0f
    case Horse(11) => 459.0f
    case _ => throw new IllegalArgumentException("unknown horse")
  }
  
  def avgPredictedVerticalForce(horse: Horse): Float = mass(horse) * 9.81f
  
  def avgPredictedRadialForce(horse: Horse, com: Point): Float = {
    val comCircle: Circle = memoizedCOMCircle(com)
    val omega: Float = angularVelocityForTrial(com)
    -omega * omega * (comCircle.radius / 1000.0f) * mass(horse)
  }
  
  def maxDeviationFromCircleAsRadiusFraction(com: Point): Float = {
    val comCircle: Circle = memoizedCOMCircle(com)
    val pts: IndexedSeq[Vec2D] = com.filter(_.isDefined).map(_.get).map(Vec2D.fromVec3Dxy(_))
    def distanceFromCircle(pt: Vec2D): Float = abs((pt - comCircle.origin).mag - comCircle.radius)
    pts.map(distanceFromCircle(_)).max / comCircle.radius
  }
  
}
