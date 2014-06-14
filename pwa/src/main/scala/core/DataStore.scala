package core

import scala.collection.immutable._
import scala.util.{Failure, Success, Try}

import java.io.File

import scalaz.\/

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


