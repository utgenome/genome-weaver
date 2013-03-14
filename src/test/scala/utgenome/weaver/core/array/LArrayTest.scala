//--------------------------------------
//
// LArrayTest.scala
// Since: 2013/03/13 13:46
//
//--------------------------------------

package utgenome.weaver.core.array

import utgenome.weaver.core.GenomeWeaverSpec
import util.Random
import utgenome.weaver.core.memory.MemoryAllocator

/**
 * @author Taro L. Saito
 */
class LArrayTest extends GenomeWeaverSpec {

  val G: Long = 1024L * 1024 * 1024

  override def afterEach {
    MemoryAllocator.default.releaseAll
  }

  "LArray" should {
    "have constructor" in {

      val l = LArray(1, 2, 3)
      val l0 = LArray()
      val l1 = LArray(1)

      try {
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
      finally {
        l.free
        l0.free
        l1.free
      }
    }

    "read/write values correctly" in {
      info("read/write test")

      val step = 1L
      val l = new LIntArray((0.1 * G).toLong)
      try {
        def v(i: Long) = (i * 2).toInt
        for(i <- 0L until(l.size, step)) l(i) = v(i)
        def loop(i: Long): Boolean = {
          if (i >= l.size)
            true
          else
            l(i) == v(i) && loop(i + step)
        }

        loop(0) should be(true)
      }
      finally {
        l.free
      }
    }




    "compare its random access performance with native Scala array and its wrapper" in {
      //val N = 1 * 1024 * 1024 * 1024
      val N = 64 * 1024 * 1024
      info("benchmark has started..")
      val arr1 = new Array[Int](N)
      val arr2 = new LIntArray(N)
      val arr3 = new LIntArraySimple(N)

      try {
        val r = new Random(0)
        val indexes = {
          val M = N / 10
          val a = new Array[Int](M)
          var i = 0
          while (i < M) {
            a(i) = r.nextInt(N);
            i += 1
          }
          a
        }
        time("random access performance", repeat = 10) {
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
      finally {
        arr2.free
        arr3.free
      }
    }

    "compare sequential access performance" in {

      //val N = 1 * 1024 * 1024 * 1024
      val N = 64 * 1024 * 1024
      info("benchmark has started..")
      val arr1 = new Array[Int](N)
      val arr2 = new LIntArray(N)
      val arr3 = new LIntArraySimple(N)

      try {
        val range = (0 until (N / 10)).map(_.toLong).toSeq
        time("sequential read performance", repeat = 5) {
          block("scala array") {
            for (i <- range)
              arr1(i.toInt)
          }

          block("LIntArray") {
            for (i <- range)
              arr2(i)

          }

          block("LIntArraySimple") {
            for (i <- range)
              arr3(i)
          }
        }
      }
      finally {
        arr2.free
        arr3.free
      }
    }


    "create large array" taggedAs ("la") in {
      for (i <- 0 until 100) {
        val arr = new LIntArray((2.1 * G).toLong)
        try {
          arr(arr.size - 1) = 134
        }
        finally {
          arr.free
        }
      }
    }

  }
}