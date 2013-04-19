package circlestudy

import org.scalatest.FunSpec

/**
 * Test for circlestudy.Bound2
 */
class Bound2Spec extends FunSpec {

  describe ("Bound2") {
    
    it ("should allow creation of a default implementation by specifying all members") {
      val b = Bound2(-1.0, 1.0, -2.0, 2.0)
      assert(b.min.x === -1.0)
      assert(b.max.x ===  1.0)
      assert(b.min.y === -2.0)
      assert(b.max.y ===  2.0)
    }
    
    it ("should allow creation of a default implementation by specifying 2 Vec2s") {
      val b = Bound2(Vec2(-1.0, -2.0), Vec2(1.0, 2.0))
      assert(b.min === Vec2(-1.0, -2.0))
      assert(b.max === Vec2(1.0, 2.0))
    }
    
    it ("should fail creation when any of the maximum values is less than the minimum values") {
      intercept [IllegalArgumentException] { Bound2(-1.0, -1.1, -2.0,  2.0) }
      intercept [IllegalArgumentException] { Bound2(-1.0,  1.0, -2.0, -2.1) }
    }
    
  }
  
}
