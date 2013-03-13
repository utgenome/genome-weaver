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

      l0.size should be (0)
      try {
        l0.apply(3)
        fail("cannot reach here")
      }
      catch {
        case e:Exception => // "OK"
      }
    }

    "use ByteBuffer" in {
      val N = 10000000
      time("array performance", repeat=10) {
        block("scala array") {
          val arr = new Array[Int](N)
          for(i <- 0 until N)
            arr(i) = 1
        }

        block("LIntArray") {
          val arr = new LIntArray(N)
          for(i <- 0 until N)
            arr(i) = 1
        }

        block("LIntArraySimple") {
          val arr = new LIntArraySimple(N)
          for(i <- 0 until N)
            arr(i) = 1
        }
      }


    }

  }
}