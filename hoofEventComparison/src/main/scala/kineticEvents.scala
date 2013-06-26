import java.io.File
import scala.collection.immutable._
import c3d._
import scala.collection.parallel.ParSeq

/**
 * KineticEvent stores a single kinetic (force plate) event.
 * 
 * @param trial trial information
 * @param onoff whether the hoof was making contact with the ground or leaving the ground
 * @param time  time since the start of the trial (0.0 = the exact start of the trial)
 */
case class KineticEvent (
  trial: TrialMetadata,
  onoff: OnOrOff,
  time: Double
)

object KineticEvents {
  
  /** Threshold of the noise floor in the C3D file. */
  val noiseFloorThreshold = 100.0
  
  /** Sampling rate was 1000 Hz for force plate (kinetic) data. */
  val samplingRate = 1000.0
  
  /** Checks if a file name belongs to a C3D file. */
  private def isC3DTrialName(name: String): Boolean = {
    val lname = name.toLowerCase
    lname.endsWith(".c3d") && (!lname.contains("static"))
  }
  
  def fromDirectory(dir: File): Seq[KineticEvent] = {
	val c3dFiles: Seq[File] = FileUtils.listFilesRecursiveStr("./data", isC3DTrialName)
    c3dFiles.flatMap(kineticEventsFromFile)
  }
  
  private def kineticEventsFromFile(file: File): Seq[KineticEvent] = {
    val fileName = file.getName
    val metadata = TrialMetadata.fromFileName(fileName)
    val c3d = C3D.read(file).getOrElse (
      throw new IllegalArgumentException(s"Could not read C3D file $fileName")
    )
    c3d.forcePlates.flatMap(kineticEventsFromForceplate(metadata, _))
  }
  
  private def kineticEventsFromForceplate(metadata: TrialMetadata, plate: ForcePlate): Seq[KineticEvent] = {
    val f: IndexedSeq[Double] = plate.force.map(_.mag.toDouble)  // force magnitude
    val belowFloor: IndexedSeq[Boolean] = f.map(_ <= noiseFloorThreshold)
    val bfzip: IndexedSeq[((Boolean,Boolean), Int)] = belowFloor.zip(belowFloor.tail).zipWithIndex
    val onEventSamples  = bfzip.filter(x => x._1._1 == true  && x._1._2 == false).map(_._2)
    val offEventSamples = bfzip.filter(x => x._1._1 == false && x._1._2 == true).map(_._2)
    val onEventTimes  = onEventSamples.map(_ / samplingRate)
    val offEventTimes = offEventSamples.map(_ / samplingRate)
    val onEvents  = onEventTimes.map(KineticEvent(metadata, OnOrOff.On, _))
    val offEvents = offEventTimes.map(KineticEvent(metadata, OnOrOff.Off, _))
    onEvents ++ offEvents
  }
  
}
