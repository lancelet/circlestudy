package pwa

import scala.annotation.tailrec
import scala.collection.immutable._
import c3d._
import c3d.util.transform.VirtualPoint
import core.DataStore.RichC3D

/**
 * Segment properties from:
 * 
 * Buchner et al. (1997) Inertial properties of Dutch Warmblood horses.  J Biomech. 30(6):653-658.
 */
object Buchner {

  /**
   * Finds the segment COM.
   * 
   * @param refA reference point A (start of reference line)
   * @param refB reference point B (end of reference line)
   * @param zAxis vector along the z-axis of the segment (normally Left to Right side of the body, but may be
   *   reversed for some right-hand segments)
   * @param relativeCOM COM from Buchner (1997), expressed relative to the reference line and segment axes
   */
  def segmentCOM(refA: Vec3D, refB: Vec3D, zAxis: Vec3D, relativeCOM: Vec3D): Vec3D = {
    
    // unit vectors (guarantee perpendicularity here)
    val ab: Vec3D = refB - refA
    val xHat: Vec3D = ab.asUnit
    val yHat: Vec3D = (zAxis cross xHat).asUnit
    val zHat: Vec3D = xHat cross yHat
    
    // reference length
    val l: Float = ab.mag
    
    // coordinates of COM (dx, dy and dz are relative to refA and mutually perpendicular)
    val dx: Vec3D = xHat * (relativeCOM.x * l)
    val dy: Vec3D = yHat * (relativeCOM.y * l)
    val dz: Vec3D = zHat * (relativeCOM.z * l)
    val com: Vec3D = refA + dx + dy + dz
    
    com
  }
  
  // Relative COM of the segments (from Table 1 of Buchner)
  object SegmentRelCOM {
    val Trunk        = Vec3D(46.9f, -10.3f,  5.0f) / 100.0f
    val Head         = Vec3D(73.0f,  -9.0f, 29.0f) / 100.0f
    val Neck         = Vec3D(46.0f,  11.0f, 16.0f) / 100.0f
    val Shoulder     = Vec3D(40.0f,   6.0f, 12.2f) / 100.0f
    val Antebrachium = Vec3D(35.0f,  -2.1f, 14.2f) / 100.0f
    val Metacarpus   = Vec3D(44.0f,  -1.0f, 17.4f) / 100.0f
    val PasternFore  = Vec3D(46.0f, -11.0f, 33.0f) / 100.0f
    val HoofFore     = Vec3D(29.0f, -20.0f, 50.0f) / 100.0f
    val Thigh        = Vec3D(59.0f, -12.0f, 20.6f) / 100.0f
    val Crus         = Vec3D(37.9f,  -8.4f, 14.0f) / 100.0f
    val Metatarsus   = Vec3D(32.0f,  -6.7f, 13.3f) / 100.0f
    val PasternHind  = Vec3D(43.0f, -13.0f, 29.0f) / 100.0f
    val HoofHind     = Vec3D(31.0f, -22.0f, 43.0f) / 100.0f
  }
  
  // Relative masses of the segments (from Table 1 of Buchner)
  object SegmentRelMass {
    private [this] val mass: Float = 538f
    val Trunk        = 352.0f  / mass
    val Head         =  23.1f  / mass
    val Neck         =  26.8f  / mass
    val Shoulder     =  20.1f  / mass
    val Antebrachium =   6.7f  / mass
    val Metacarpus   =   1.59f / mass
    val PasternFore  =   0.73f / mass
    val HoofFore     =   1.08f / mass
    val Thigh        =  18.6f  / mass
    val Crus         =   8.3f  / mass
    val Metatarsus   =   2.84f / mass
    val PasternHind  =   0.89f / mass
    val HoofHind     =   0.99f / mass
  }
  
  /**
   * Finds a vector running from the left to right side of the body in a given trial.
   * 
   * This assumes that the horse is standing square.  Should typically be called only on a static trial.
   */
  def leftToRight(c3d: C3D, 
      markerNames: Seq[String] = Seq("Atlas", "TuberCoxaeUpper", "TuberCoxae", "Rib1", "Rib2", "Hip")): Vec3D = 
  {
    def avg(name: String): Vec3D = PWA.averagePt(c3d.getCSPoint(name).get)
    def lr(name: String): Vec3D = (avg(s"R$name") - avg(s"L$name")).asUnit
    markerNames.map(lr(_)).foldLeft(Vec3D(0,0,0))(_ + _).asUnit
  }

  trait MassiveBody {
    def point(static: C3D, c3d: C3D): Point
    def relativeMass: Float
  }

  final class MassiveBodyImpl(name: String, refA: Seq[String], refB: Seq[String], relCOM: Vec3D,
    val relativeMass: Float, cloud: IndexedSeq[String], flipLtoR: Boolean) extends MassiveBody
  {
    def point(static: C3D, c3d: C3D): Point = {
      def sAvg(name: String): Vec3D = PWA.averagePt(static.getCSPoint(name).get)
      def ssAvg(names: Seq[String]): Vec3D = names.map(sAvg).foldLeft(Vec3D(0,0,0))(_ + _) / names.length
      
      // find COM in the static trial
      val zAxis: Vec3D = leftToRight(static) * (if (flipLtoR) -1.0f else 1.0f)
      val staticCOM: Vec3D = segmentCOM(ssAvg(refA), ssAvg(refB), zAxis, relCOM)

      // find the static cloud of marker points
      val staticCloud: IndexedSeq[Vec3D] = for {
        pointName <- cloud
        avg: Vec3D = sAvg(pointName)
      } yield {
        if (avg.x.isNaN || avg.y.isNaN || avg.z.isNaN) {
          println(s"point $pointName had a NaN average in trial ${static.source}")
        }
        avg
      }
      
      // track the COM with a virtual point
      //val staticCloud: IndexedSeq[Vec3D] = cloud.map(sAvg(_))
      val trialCloud: IndexedSeq[Point] = cloud.map(c3d.getCSPoint(_).get)
      VirtualPoint(name, name, staticCOM, staticCloud, trialCloud)
    }
  }

  object MassiveBody {
    
    def apply(name: String, refA: Seq[String], refB: Seq[String], relCOM: Vec3D, relativeMass: Float,
      cloud: IndexedSeq[String], flipLtoR: Boolean): MassiveBody = 
        new MassiveBodyImpl(name, refA, refB, relCOM, relativeMass, cloud, flipLtoR)
    
    def apply(name: String, refA: String, refB: String, relCOM: Vec3D, relativeMass: Float, 
      cloud: IndexedSeq[String], flipLtoR: Boolean): MassiveBody = 
        new MassiveBodyImpl(name, Seq(refA), Seq(refB), relCOM, relativeMass, cloud, flipLtoR)
    
    def lr(name: String, refA: Seq[String], refB: Seq[String], relCOM: Vec3D, relativeMass: Float,
      cloud: IndexedSeq[String], left: Boolean): MassiveBody =
    {
      val p: String = if (left) "L" else "R"
      def addPrefix(s: String): String = s"$p$s"
      val namePref: String = addPrefix(name)
      val refApref: Seq[String] = refA.map(addPrefix(_))
      val refBpref: Seq[String] = refB.map(addPrefix(_))
      val cloudPref: IndexedSeq[String] = cloud.map(addPrefix(_))
      apply(namePref, refApref, refBpref, relCOM, relativeMass, cloudPref, !left)
    }
    
    def lr(name: String, refA: String, refB: String, relCOM: Vec3D, relativeMass: Float,
      cloud: IndexedSeq[String], left: Boolean): MassiveBody =
        lr(name, Seq(refA), Seq(refB), relCOM, relativeMass, cloud, left)
    
  }
  
  // Massive bodies
  object MB {
    
    // unique bodies
    val Trunk = MassiveBody("TrunkCOM", Seq("S4"), Seq("LC6", "RC6"), SegmentRelCOM.Trunk, SegmentRelMass.Trunk,
      IndexedSeq("T6", "T8", "T12", "T16", "L3", "L6", "S2", "S4", "LC6", "RC6", "LRib1", "RRib1", "LRib2", "RRib2",
                 "LTuberCoxaeUpper", "RTuberCoxaeUpper", "LTuberCoxae", "RTuberCoxae", "LHip", "RHip",
                 "Belly1", "Belly2"), false)
    val Head = MassiveBody("HeadCOM", Seq("LAtlas"), Seq("LFacialCrest"),
      SegmentRelCOM.Head, SegmentRelMass.Head, IndexedSeq("LFacialCrest", "RFacialCrest", "Forehead"), false)
    val Neck = MassiveBody("NeckCOM", Seq("LC6"), Seq("LAtlas"), SegmentRelCOM.Neck,
      SegmentRelMass.Neck, IndexedSeq("LC6", "RC6", "LAtlas", "RAtlas"), false)

    // symmetric (L-R) bodies
    // Left
    val LShoulder = MassiveBody.lr("ShoulderCOM", "Scapula", "HumerusDist", SegmentRelCOM.Shoulder,
      SegmentRelMass.Shoulder, IndexedSeq("Scapula", "Shoulder", "HumerusDist"), true)
    val LAntebrachium = MassiveBody.lr("AntebrachiumCOM", "HumerusDist", "CarpusLat", SegmentRelCOM.Antebrachium,
      SegmentRelMass.Antebrachium, IndexedSeq("RadiusProx", "RadiusDorsal", "RadiusDist", "RadiusDistMed"), true)
    val LMetacarpus = MassiveBody.lr("MetacarpusCOM", "CarpusLat", "FFetlockLat", SegmentRelCOM.Metacarpus,
      SegmentRelMass.Metacarpus, IndexedSeq("MetacarpusDorsal", "FFetlockLat", "CarpusLat", "CarpusMed"), true)
    val LPasternFore = MassiveBody.lr("PasternForeCOM", "FFetlockLat", "FHoofLatTop", SegmentRelCOM.PasternFore,
      SegmentRelMass.PasternFore, IndexedSeq("FFetlockLat", "FPasternMed", "FPasternLat"), true)
    val LHoofFore = MassiveBody.lr("HoofForeCOM", "FHoofLatTop", "FHoofLatBottom", SegmentRelCOM.HoofFore,
      SegmentRelMass.HoofFore, IndexedSeq("FHoofLatTop", "FHeel", "FHoofLatBottom", "FHoofDorsal"), true)
    val LThigh = MassiveBody.lr("ThighCOM", "Hip", "FemurDist", SegmentRelCOM.Thigh,
      SegmentRelMass.Thigh, IndexedSeq("TuberCoxae", "Hip", "FemurDist"), true)  // not great segment
    val LCrus = MassiveBody.lr("CrusCOM", "FemurDist", "TarsusLat", SegmentRelCOM.Crus,
      SegmentRelMass.Crus, IndexedSeq("TibiaProx", "TibiaDorsal", "TibiaDist", "TibiaDistMed"), true)
    val LMetatarsus = MassiveBody.lr("MetatarsusCOM", "TarsusLat", "HFetlockLat", SegmentRelCOM.Metatarsus,
      SegmentRelMass.Metatarsus, IndexedSeq("TarsusLat", "TarsusMed", "MetatarsusDorsal", "HFetlockLat"), true)
    val LPasternHind = MassiveBody.lr("PasternHindCOM", "HFetlockLat", "HHoofLatTop", SegmentRelCOM.PasternHind,
      SegmentRelMass.PasternHind, IndexedSeq("HFetlockLat", "HPasternMed", "HPasternLat"), true)
    val LHoofHind = MassiveBody.lr("HoofHindCOM", "HHoofLatTop", "HHoofLatBottom", SegmentRelCOM.HoofHind,
      SegmentRelMass.HoofHind, IndexedSeq("HHoofLatTop", "HHoofLatBottom", "HHoofDorsal", "HHeel"), true)
    private [this] val leftBodies = Seq(LShoulder, LAntebrachium, LMetacarpus, LPasternFore, LHoofFore,
      LThigh, LCrus, LMetatarsus, LHoofHind)
    // Right    
    val RShoulder = MassiveBody.lr("ShoulderCOM", "Scapula", "HumerusDist", SegmentRelCOM.Shoulder,
      SegmentRelMass.Shoulder, IndexedSeq("Scapula", "Shoulder", "HumerusDist"), false)
    val RAntebrachium = MassiveBody.lr("AntebrachiumCOM", "HumerusDist", "CarpusLat", SegmentRelCOM.Antebrachium,
      SegmentRelMass.Antebrachium, IndexedSeq("RadiusProx", "RadiusDorsal", "RadiusDist", "RadiusDistMed"), false)
    val RMetacarpus = MassiveBody.lr("MetacarpusCOM", "CarpusLat", "FFetlockLat", SegmentRelCOM.Metacarpus,
      SegmentRelMass.Metacarpus, IndexedSeq("MetacarpusDorsal", "FFetlockLat", "CarpusLat", "CarpusMed"), false)
    val RPasternFore = MassiveBody.lr("PasternForeCOM", "FFetlockLat", "FHoofLatTop", SegmentRelCOM.PasternFore,
      SegmentRelMass.PasternFore, IndexedSeq("FFetlockLat", "FPasternMed", "FPasternLat"), false)
    val RHoofFore = MassiveBody.lr("HoofForeCOM", "FHoofLatTop", "FHoofLatBottom", SegmentRelCOM.HoofFore,
      SegmentRelMass.HoofFore, IndexedSeq("FHoofLatTop", "FHeel", "FHoofLatBottom", "FHoofDorsal"), false)
    val RThigh = MassiveBody.lr("ThighCOM", "Hip", "FemurDist", SegmentRelCOM.Thigh,
      SegmentRelMass.Thigh, IndexedSeq("TuberCoxae", "Hip", "FemurDist"), false)  // not great segment
    val RCrus = MassiveBody.lr("CrusCOM", "FemurDist", "TarsusLat", SegmentRelCOM.Crus,
      SegmentRelMass.Crus, IndexedSeq("TibiaProx", "TibiaDorsal", "TibiaDist", "TibiaDistMed"), false)
    val RMetatarsus = MassiveBody.lr("MetatarsusCOM", "TarsusLat", "HFetlockLat", SegmentRelCOM.Metatarsus,
      SegmentRelMass.Metatarsus, IndexedSeq("TarsusLat", "TarsusMed", "MetatarsusDorsal", "HFetlockLat"), false)
    val RPasternHind = MassiveBody.lr("PasternHindCOM", "HFetlockLat", "HHoofLatTop", SegmentRelCOM.PasternHind,
      SegmentRelMass.PasternHind, IndexedSeq("HFetlockLat", "HPasternMed", "HPasternLat"), false)
    val RHoofHind = MassiveBody.lr("HoofHindCOM", "HHoofLatTop", "HHoofLatBottom", SegmentRelCOM.HoofHind,
      SegmentRelMass.HoofHind, IndexedSeq("HHoofLatTop", "HHoofLatBottom", "HHoofDorsal", "HHeel"), false)
    private [this] val rightBodies = Seq(RShoulder, RAntebrachium, RMetacarpus, RPasternFore, RHoofFore,
      RThigh, RCrus, RMetatarsus, RHoofHind)
      
    // list of all bodies
    val bodies: Seq[MassiveBody] = Seq(Trunk, Head, Neck) ++ leftBodies ++ rightBodies
      
  }
  
  /**
   * Finds the body Centre of Mass.
   */
  def bodyCOM(static: C3D, c3d: C3D): Point = {
    new Point {
      private [this] val bodies: IndexedSeq[MassiveBody] = MB.bodies.to[IndexedSeq]
      private [this] val points: IndexedSeq[Point] = bodies.map(_.point(static, c3d))
      private [this] val masses: IndexedSeq[Float] = bodies.map(_.relativeMass)
      private [this] val totalMass: Float = masses.sum
      
      val name: String = "BodyCOM"
      val description: String = "Body Center of Mass"
      val rate: Float = points.head.rate
      def asMarker: Marker = throw new NotImplementedError("asMarker not implemented")
      val length: Int = points.head.length
      def apply(idx: Int): Option[Vec3D] = {
        @tailrec def avg(x: Float, y: Float, z: Float, ptIndex: Int): Option[Vec3D] = {
          if (ptIndex == points.length) {
            Some(Vec3D(x / totalMass, y / totalMass, z / totalMass))
          } else {
            points(ptIndex).apply(idx) match {
              case None    => None
              case Some(v) => {
                val mass = masses(ptIndex)
                avg(x + (v.x * mass), y + (v.y * mass), z + (v.z * mass), ptIndex + 1)
              }
            }
          }
        }
        avg(0, 0, 0, 0)
      }
    }
  }

}