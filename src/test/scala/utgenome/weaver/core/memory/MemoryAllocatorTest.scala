//--------------------------------------
//
// MemoryAllocatorTest.scala
// Since: 2013/03/14 3:33 PM
//
//--------------------------------------

package utgenome.weaver.core.memory

import utgenome.weaver.core.GenomeWeaverSpec

/**
 * @author Taro L. Saito
 */
class MemoryAllocatorTest extends GenomeWeaverSpec {

  "UnsafeAllocater" should {

    "create arrays whose memory can be released" in {

      val m = new UnsafeAllocator
      val arr = m.newIntArray(10)
      arr.size should be (10)
      for(i <- 0 until arr.size) {
        arr(i) = i
      }

      for(i <- 0 until arr.size) {
        arr(i) should be (i)
      }

      m.releaseAll
    }

  }

}