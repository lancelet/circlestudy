package circlestudy.trials

import java.io.File
import scalaz.Validation

/** Database of trials. */
trait TrialDatabase {
  def getTrial(metadata: TrialDatabase.Metadata): Validation[String, Trial]
}

object TrialDatabase {
  
  sealed trait Curvature
  object Curvature {    
    case class Circle(direction: Direction) extends Curvature
	object Straight extends Curvature
	
    sealed trait Direction
    object Direction {
      object Left  extends Direction
      object Right extends Direction      
    }	
  }

  sealed trait Gait
  object Gait {
    object Walk extends Gait
    object Trot extends Gait    
  }
  
  case class Metadata(horseNumber: Int, curvature: Curvature, gait: Gait, trialNumber: Int)  
}

/** Database of TRC trials. */
object TRCTrialDatabase extends TrialDatabase {
  import TrialDatabase._
  import Curvature._
  import Direction._
  import Gait._
  
  def getTrial(metadata: Metadata): Validation[String, Trial] = {
    val curveDir = metadata.curvature match {
      case Straight  => "Straight"
      case Circle(_) => "Circle"
    }
    val curveFile = metadata.curvature match {
      case Straight      => "straight"
      case Circle(Left)  => "circle_left"
      case Circle(Right) => "circle_right"
    }
    var gaitTxt = metadata.gait match {
      case Walk => "walk"
      case Trot => "trot"
    }
    val dirName = s"data/Horse${metadata.horseNumber}/${curveDir}"
    val fileName = s"Horse${metadata.horseNumber}_${curveFile}_${gaitTxt}_${metadata.trialNumber}.trc"
    
    Trial.fromTRCFile(new File(s"${dirName}/${fileName}"))
  }
  
}
