package circlestudy

import org.scalatest.FunSpec

/**
 * Test for circlesutdy.Vec2
 */
class Vec2Spec extends FunSpec {

  describe ("Vec2") {
    
    it ("should allow creation of a default implementation") {
      val v = Vec2(1.0, 2.0)
      assert(v.x === 1.0)
      assert(v.y === 2.0)
    }
    
    it ("should allow equality testing between different implementations") {
      case class MyVec2(x: Double, y: Double) extends Vec2
      assert(MyVec2(1.0, 2.0) === Vec2(1.0, 2.0))
      assert(MyVec2(1.1, 2.0) != Vec2(1.0, 2.0))
      assert(MyVec2(1.0, 2.1) != Vec2(1.0, 2.0))
    }
    
  }
  
}