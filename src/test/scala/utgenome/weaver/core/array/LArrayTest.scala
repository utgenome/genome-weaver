//--------------------------------------
//
// LArrayTest.scala
// Since: 2013/03/13 13:46
//
//--------------------------------------

package utgenome.weaver.core.array

import utgenome.weaver.core.GLensSpec

/**
 * @author Taro L. Saito
 */
class LArrayTest extends GLensSpec {

  "LArray" should {
    "have constructor" in {

      val l = LArray(1, 2, 3)
      val l0 = LArray()
      val l1 = LArray(1)

      l.size should be(3)
      l(0) should be (1)
      l(1) should be (2)
      l(2) should be (3)


    }

  }
}