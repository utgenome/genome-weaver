//--------------------------------------
//
// LArrayTest.scala
// Since: 2013/03/13 13:46
//
//--------------------------------------

package utgenome.weaver.core.array

import utgenome.weaver.core.GLensSpec
import util.Random

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
      l(0) should be(1)
      l(1) should be(2)
      l(2) should be(3)

      l0.size should be(0)
      try {
        l0.apply(3)
        fail("cannot reach here")
      }
      catch {
        case e: Exception => // "OK"
      }
    }

    "use ByteBuffer" in {
      val N = 1 * 1024 * 1024 * 1024
      info("benchmark started..")
      val arr1 = new Array[Int](N)
      val arr2 = new LIntArray(N)
      val arr3 = new LIntArraySimple(N)

      val r = new Random(0)
      val indexes = {
        val a = Array.ofDim[Int](N/4)
        var i = 0
        val M =  N / 4
        while(i < M) { a(i) = r.nextInt(N); i += 1 }
        a
      }
      info("here")
      try {
        time("array performance", repeat = 3) {
          block("scala array") {
            for (i <- indexes)
              arr1(i) = 1
          }

          block("LIntArray") {
            for (i <- indexes)
              arr2(i) = 1

          }

          block("LIntArraySimple") {
            for (i <- indexes)
              arr3(i) = 1
          }
        }
      }
      finally
        arr2.free
    }

    "create large array" in {
      val G: Long = 1024L * 1024 * 1024
      val arr = new LIntArray(4 * G)
      try {
        arr(3 * G) = 134
      }
      finally {
        arr.free
        //LArray.free(arr)
      }
    }

  }
}