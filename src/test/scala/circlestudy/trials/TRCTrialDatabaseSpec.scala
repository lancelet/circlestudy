package circlestudy.trials

import org.scalatest.FunSpec

class TRCTrialDatabaseSpec extends FunSpec {
  
  describe("TRCTrialDatabase") {
    
    it ("should fail to load a non-existent TRC file") {
      val curvature = TrialDatabase.Curvature.Circle(TrialDatabase.Curvature.Direction.Left)
      val gait      = TrialDatabase.Gait.Trot
      TRCTrialDatabase getTrial(TrialDatabase.Metadata(42, curvature, gait, 1024)) fold (
        error => { /* error is expected */ },
        trial => fail("an error was expected when loading a non-existent trial")
      )
    }
    
    it ("should be able to access an example TRC file") {
      val curvature = TrialDatabase.Curvature.Circle(TrialDatabase.Curvature.Direction.Left)
      val gait      = TrialDatabase.Gait.Trot
      TRCTrialDatabase getTrial(TrialDatabase.Metadata(3, curvature, gait, 3)) fold (
        error => {
          println(s"error: ${error}")
          fail("could not load example TRC file (do you have the McPhail data?)")
        },
        trial => {
          assert(trial.markers.length === 92)
        }
      )
    }
    
  }
  
}