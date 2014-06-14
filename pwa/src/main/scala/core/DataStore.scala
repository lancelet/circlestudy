package core

import scala.collection.immutable._
import scala.util.{Failure, Success, Try}

import java.io.{IOException, File}

import scalaz.\/
import scalaz.Scalaz.ToIdOps  // provides .left and .right implicits

import c3d.C3D


/** Horse. */
final case class Horse(id: Int)

/** Single motion (non-static) trial. */
trait MotionTrial {
  def direction: Direction
  def gait: Gait
  def c3d: Throwable \/ C3D
}

/** Direction of a trial: straight, circle right or circle left. */
sealed trait Direction {
  import Direction._
  def isCircle: Boolean = (this == CircleLeft) || (this == CircleRight)
}
object Direction {
  case object Straight    extends Direction { override def toString = "straight"     }
  case object CircleRight extends Direction { override def toString = "circle right" }
  case object CircleLeft  extends Direction { override def toString = "circle left"  }
}

/** Gait for a trial: walk or trot. */
sealed trait Gait
object Gait {
  case object Walk extends Gait { override def toString = "walk" }
  case object Trot extends Gait { override def toString = "trot" }
}


/**
 * Represents the total store of data for the project.
 *
 * @param dataDir  top-level data directory
 */
class DataStore(dataDir: File = new File("/Users/jsm/Documents/dev/circlestudy/data")) {

  /**
   * Finds the motion (non-static) trials for a given horse.
   *
   * @param horse horse for which to obtain the motion trials
   * @return sequence of motion trials
   */
  def motionTrials(horse: Horse): Seq[MotionTrial] = {

    // determines whether a file name matches one of the motion trials for the given horse
    def fileNameMatchesHorse(name: String): Boolean = {
      val n = name.toLowerCase
      n.startsWith(s"horse${horse.id}") &&
      (n.contains("walk") || n.contains("trot")) &&
      n.endsWith(".c3d")
    }

    // constructs a MotionTrial for a particular file
    def motionTrialFromFile(f: File) = {
      import Direction._
      import Gait._
      def has(s: String) = f.getName.toLowerCase.contains(s)
      new MotionTrial {
        val direction: Direction = if      (has("left"))  CircleLeft
                                   else if (has("right")) CircleRight
                                   else                   Straight
        val gait: Gait = if (has("walk")) Walk else Trot
        def c3d: Throwable \/ C3D = \/.fromTryCatch( C3D.read(f) )
      }
    }

    DataStore
      .deepFileSearch(dataDir, fileNameMatchesHorse)
      .map(motionTrialFromFile)
  }

  /**
   * Finds the static C3D trial for a horse.
   *
   * @param horse horse for which to find the static trial
   * @return either the static C3D file or the error produced when attempting to read it
   */
  def staticTrial(horse: Horse): Throwable \/ C3D = {

    def fileNameMatchesHorse(name: String): Boolean = {
      val n = name.toLowerCase
      n.startsWith(s"horse${horse.id}") && name.contains("static_virtual")
    }

    val files = DataStore.deepFileSearch(dataDir, fileNameMatchesHorse)

    // there should be just one static virtual file; if not then create some exceptions
    if (files.length > 1) {
      new IOException(s"more than one static virtual trial found for horse ${horse.id}").left
    } else if (files.length == 0) {
      new IOException(s"no static virtual trial found for horse ${horse.id}").left
    } else {
      \/.fromTryCatch( C3D.read(files.head) )
    }

  }

  /**
   * Returns the complete list of horses for which there are trials.
   *
   * If `useOnly` is provided then only the horses which appear in that list and also in the actual
   * set of trials are used.
   *
   * @param useOnly return only these horses if they have trials
   * @return sequence of horses
   */
  def horses(useOnly: Seq[Horse] = Seq.empty[Horse]): Seq[Horse] = {

    def allHorsesOnDisk: Seq[Horse] = {
      val subdirs: Seq[File] = dataDir.listFiles.to[Seq].filter(_.isDirectory)
      val prefix = "Horse "
      val numberStrings: Seq[String] = subdirs
        .map(_.getName)
        .filter(_.startsWith(prefix))
        .map(_.drop(prefix.length))
      numberStrings.map(_.toInt).map(Horse).sortBy(_.id)
    }

    val ids = useOnly.map(_.id)
    def keepHorse(h: Horse): Boolean = if (useOnly.isEmpty) true else ids.contains(h.id)

    allHorsesOnDisk.filter(keepHorse)

  }

}


object DataStore {

  /**
   * Searches through all sub-directories for files whose names match a given predicate.
   *
   * @param parentDir parent directory
   * @param namePredicate predicate for the names
   * @return files that match the given name
   */
  def deepFileSearch(parentDir: File, namePredicate: String => Boolean): Seq[File] = {
    val dirFiles = parentDir.listFiles.to[Seq]
    val matchingFiles = dirFiles.filter { f: File => namePredicate(f.getName) }
    val subdirs = dirFiles.filter(_.isDirectory)
    matchingFiles ++ subdirs.flatMap(deepFileSearch(_, namePredicate))
  }

}


