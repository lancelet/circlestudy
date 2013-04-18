package circlestudy.trials.mocaputils

import org.scalatest.FunSpec
import java.net.URL
import java.io.File

/**
 * Test for circlestudy.trials.mocaputils.MocapUtilsTrial
 */
class MocapUtilsTrialSpec extends FunSpec {

  describe("MocapUtilsTrial") {

    it ("should fail on a non-existent file") {
      val result = MocapUtilsTrial.fromTRCFile(new File("resources/not-a-c3d-file.c3d"))
      result fold (
        error => {
          /* error is expected */
        },
        success => fail("expected to fail when reading a non-existent file")
      )
    }

    it ("should correctly read a TRC file") {
      val trcFile: File = {
        val url: URL = getClass.getResource("testFile.trc")
        val file: File = new File(url.toURI.getPath)
        println(file.getAbsolutePath)
        println(file.exists)
        file
      }
      MocapUtilsTrial.fromTRCFile(trcFile) fold (
        error => fail(error),
        trial => {
          assert(trial.markers.length === 9)

          val markerNames = trial.markers.map(_.name).sorted
          val expectedNames = List("Head", "Rein1", "RFHoofL", "LFHoofL", "Surcingle", "S2", "RHHoofL", "LHHoofL",
            "Rein2").sorted
          assert(markerNames === expectedNames)

          // check head marker
          val expectedCoords = Vector(
            trial.Vec3(1247.12988, 649.67725, 1446.53552),
            trial.Vec3(1247.26807, 650.12024, 1447.11145),
            trial.Vec3(1246.90698, 650.64636, 1447.61157),
            trial.Vec3(1246.85059, 650.76019, 1447.70544),
            trial.Vec3(1246.60803, 651.06555, 1447.99841)
          ).map(Some(_)).toList
          val headMarker = trial.markers.find(_.name == "Head").getOrElse(fail("Head marker not found!"))
          assert(headMarker.co === expectedCoords)
          assert(headMarker.sampleFreq === 60.0)
        }
      )
    }

  }

}
