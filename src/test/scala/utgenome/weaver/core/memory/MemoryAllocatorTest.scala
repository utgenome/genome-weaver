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
      pending // probably impossible to imitate Array using Unsafe
      val m = new UnsafeAllocator
      m.releaseAll
    }

  }

}