package circlestudy

import org.scalatest.FunSpec
import scala.collection.immutable._

/**
 * Test for circlestudy.Bound3
 */
class Bound3Spec extends FunSpec {

  describe ("Bound3") {

    it ("should allow creation of a default implementation by specifying all members") {
      val b = Bound3(-1.0, 1.0, -2.0, 2.0, -3.0, 3.0)
      assert(b.min.x === -1.0)
      assert(b.max.x ===  1.0)
      assert(b.min.y === -2.0)
      assert(b.max.y ===  2.0)
      assert(b.min.z === -3.0)
      assert(b.max.z ===  3.0)
    }

    it ("should allow creation of a default implementation by specifying 2 Vec3s") {
      val b = Bound3(Vec3(-1.0, -2.0, -3.0), Vec3(1.0, 2.0, 3.0))
      assert(b.min === Vec3(-1.0, -2.0, -3.0))
      assert(b.max === Vec3(1.0, 2.0, 3.0))
    }

    it ("should fail creation when any of the maximum values is less than the minimum values") {
      intercept[IllegalArgumentException] { Bound3(-1.0, -1.1, -2.0,  2.0, -3.0,  3.0) }
      intercept[IllegalArgumentException] { Bound3(-1.0,  1.0, -2.0, -2.1, -3.0,  3.0) }
      intercept[IllegalArgumentException] { Bound3(-1.0,  1.0, -2.0,  2.0, -3.0, -3.1) }
    }

    it ("should allow creation from a sequence of bounding boxes") {
      val boundSeq = Seq(Bound3(-1.0, 1.0, -2.0, 2.0, -3.0, 3.0), Bound3(-1.0, 1.5, -2.5, 1.0, -3.5, 3.5))
      val b = Bound3(boundSeq)
      assert(b.min === Vec3(-1.0, -2.5, -3.5))
      assert(b.max === Vec3( 1.5,  2.0,  3.5))
    }

  }

}
