package circlestudy

import org.scalatest.FunSpec

/**
 * Test for circlestudy.Vec3
 */
class Vec3Spec extends FunSpec {

  describe ("Vec3") {

    it ("should allow creation of a default implementation") {
      val v = Vec3(1.0, 2.0, 3.0)
      assert(v.x === 1.0)
      assert(v.y === 2.0)
      assert(v.z === 3.0)
    }

    it ("should allow equality testing between different implementations") {
      case class MyVec3(x: Double, y: Double, z: Double) extends Vec3
      assert(MyVec3(1.0, 2.0, 3.0) === Vec3(1.0, 2.0, 3.0))
      assert(MyVec3(1.1, 2.0, 3.0) != Vec3(1.0, 2.0, 3.0))
      assert(MyVec3(1.0, 2.1, 3.0) != Vec3(1.0, 2.0, 3.0))
      assert(MyVec3(1.0, 2.0, 2.9) != Vec3(1.0, 2.0, 3.0))
    }

  }

}