import java.io.File
import java.io.FileFilter
import java.io.FilenameFilter
import scala.collection.immutable._
import scala.io.Source

// LeftOrRight refers to Left vs Right limb
sealed trait LeftOrRight
object LeftOrRight {
  case object Left  extends LeftOrRight
  case object Right extends LeftOrRight
}

// ForeOrHind refers to Fore (thoracic) vs Hind (pelvic) limb
sealed trait ForeOrHind
object ForeOrHind {
  case object Fore extends ForeOrHind
  case object Hind extends ForeOrHind
}

// OnOrOff refers to a hoof making contact with the ground (On) or leaving the ground (Off)
sealed trait OnOrOff
object OnOrOff {
  case object On  extends OnOrOff
  case object Off extends OnOrOff
}

/**
 * HoofKinematicEvent stores a single kinematic event.
 * 
 * @param trial      trial information
 * @param laterality whether the left or the right limb was involved
 * @param forehind   whether the fore or hind limb was involved
 * @param onoff      whether the hoof was making contact with the ground or leaving the ground
 * @param time       time since the start of the trial (0.0 = the exact start of the trial)
 */
case class HoofKinematicEvent (
  trial: TrialMetadata, 
  laterality: LeftOrRight, 
  forehind: ForeOrHind,
  onoff: OnOrOff,
  time: Double
)

object HoofKinematicEvent {
  
  def findLeg(eventFileName: String): (LeftOrRight, ForeOrHind) = {
    import LeftOrRight._
    import ForeOrHind._
    if      (eventFileName.contains("LF")) (Left,  Fore)
    else if (eventFileName.contains("LH")) (Left,  Hind)
    else if (eventFileName.contains("RF")) (Right, Fore)
    else if (eventFileName.contains("RH")) (Right, Hind)
    else {
      throw new IllegalArgumentException("No hoof identifier (LF, LH, RF, RH) found in the event file name.")
    }
  }
  
  def findOnOrOff(eventFileName: String): OnOrOff = {
    import OnOrOff._
    if (eventFileName.toLowerCase.contains("on")) On else Off
  }
  
}

object KinematicEvents {
  
  /** Checks if a file name belongs to an offset (range) file. */
  private def isOffsetFileName(name: String): Boolean = name.endsWith("_range.txt")
  
  /** Checks if a file name belongs to an event file. */
  private def isEventFileName(name: String): Boolean = {
    val lname = name.toLowerCase
    lname.endsWith("on.txt") || name.endsWith("off.txt")
  }
  
  /**
   * Trawls through a top-level directory and all of its sub-directories, finding all of the _range.txt files and 
   * extracting offsets from them.  It returns a map so that a particular trial (as identified by its complete set
   * of metadata) can be used to find the integer offset.
   * 
   * @param dir top-level directory
   * @return map containing TrialMetadata as keys and offsets as integers
   */
  private def readAllOffsets(dir: File): Map[TrialMetadata,Int] = {
    assert(dir.isDirectory)
    val rangeFiles: Seq[File] = FileUtils.listFilesRecursiveStr("./data", isOffsetFileName)
    val mapSeq: Seq[(TrialMetadata, Int)] = for (rangeFile <- rangeFiles) yield {
      val offset = FileUtils.read(rangeFile).takeWhile(_.isDigit).toInt  // read offset from file itself
      val metadata = TrialMetadata.fromFileName(rangeFile.getName)       // metadata from file name
      (metadata, offset - 1)
    }
    val map = mapSeq.toMap
    assert(map.size == mapSeq.length, "Duplicate offset data was found when converting to a map!")
    return map
  }
  
  /** Sampling rate was 100 Hz for point (kinematic) data. */
  val samplingRate: Double = 100.0
  
  /**
   * Trawls through a top-level directory and all of its sub-directories, finding all of the hoof kinematic events
   * recorded.
   * 
   * @param dir top-level directory
   * @return sequence of HoofKinematicEvents found
   */
  def fromDirectory(dir: File): Seq[HoofKinematicEvent] = {
    assert(dir.isDirectory)
    
    val offsets: Map[TrialMetadata, Int] = readAllOffsets(dir)
        
    val eventFiles: Seq[File] = FileUtils.listFilesRecursiveStr("./data", isEventFileName)
    
    val eventSubSeq: Seq[Seq[HoofKinematicEvent]] = for (file <- eventFiles) yield {
      val fileName = file.getName
      val metadata = TrialMetadata.fromFileName(fileName)
      val (laterality, forehind) = HoofKinematicEvent.findLeg(fileName)
      val onoff = HoofKinematicEvent.findOnOrOff(fileName)
      val offset = offsets.get(metadata).getOrElse(
        throw new IllegalArgumentException(s"Offset not found for trial ${metadata.toString}")
      )
      val fileString = FileUtils.read(file).replaceAll("\n", "")
      val eventSamples = fileString.split(',').to[Seq].map(_.toInt).map(_ + offset)
      val eventTimes = eventSamples.map(_ / samplingRate)
      eventTimes.map(HoofKinematicEvent(metadata, laterality, forehind, onoff, _))
    }
    
    eventSubSeq.flatten
  }
    
}
