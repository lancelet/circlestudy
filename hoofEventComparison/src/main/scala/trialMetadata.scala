
// Direction used for circular locomotion
sealed trait CircleDirection
object CircleDirection {
  case object Left  extends CircleDirection
  case object Right extends CircleDirection
}

// Whether locomotion was circular or straight
sealed trait CircleOrStraight
object CircleOrStraight {
  case class Circle(direction: CircleDirection) extends CircleOrStraight
  case object Straight extends CircleOrStraight
}

// Gait (walk or trot)
sealed trait WalkOrTrot
object WalkOrTrot {
  case object Walk extends WalkOrTrot
  case object Trot extends WalkOrTrot
}

/**
 * Information about a trial.
 * 
 * @param horseNumber      number identifier of the horse
 * @param circleOrStraight was the trial circular or in a straight line (and if circular, what direction)?
 * @param walkOrTrot       was the trial walking or trotting
 * @param trialNumber      for the particular combination of conditions, which trial number?
 */
case class TrialMetadata(
  horseNumber: Int, 
  circleOrStraight: CircleOrStraight, 
  walkOrTrot: WalkOrTrot, 
  trialNumber: Int
)

object TrialMetadata {
  
  def findHorseNumber(fileName: String): Int = {
    val offset = fileName.toLowerCase.indexOf("horse")
    s"${fileName(offset + 5)}".toInt
  }
	
  def findCircleOrStraight(fileName: String): CircleOrStraight = {
    if (fileName.contains("circle")) {
      if (fileName.contains("left"))
        CircleOrStraight.Circle(CircleDirection.Left)
      else
        CircleOrStraight.Circle(CircleDirection.Right)
    } else {
      CircleOrStraight.Straight
    }
  }
	
  def findWalkOrTrot(fileName: String): WalkOrTrot = {
    if (fileName.contains("walk"))
      WalkOrTrot.Walk
    else
      WalkOrTrot.Trot
  }
	
  def findTrialNumber(fileName: String): Int = {
    // assumes that the trial number follows immediately after "walk_" or "trot_"; can be 1 or 2 digits
    val walkOrTrotString = if (fileName.contains("walk")) "walk" else "trot"
    val startOffset = fileName.indexOf(walkOrTrotString) + 5
    val trialNumberString = fileName.drop(startOffset).takeWhile(_.isDigit)
    try {
      trialNumberString.toInt
    } catch {
      case nfe: NumberFormatException => {
        println(s"Could not read trial number for file $fileName")
        throw nfe
      }
    }
  }  

  def fromFileName(fileName: String): TrialMetadata = new TrialMetadata(
    findHorseNumber(fileName),
    findCircleOrStraight(fileName),
    findWalkOrTrot(fileName),
    findTrialNumber(fileName)
  )
  
}
