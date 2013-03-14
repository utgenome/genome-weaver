//--------------------------------------
//
// MemoryAllocator.scala
// Since: 2013/03/14 9:26
//
//--------------------------------------

package utgenome.weaver.core.memory

import sun.misc.Unsafe

object MemoryAllocator {


}


/**
 * Memory allocator interface
 *
 * @author Taro L. Saito
 */
trait MemoryAllocator {

  /**
   * Allocate a memory of the specified byte length. The allocated memory must be released via [[utgenome.weaver.core.memory.MemoryAllocator#release]]
   * as in malloc() in C/C++.
   * @param size byte length of the memory
   * @return adress of the allocated mmoery.
   */
  def allocate(size:Long) : Long

  /**
   * Release the memory allocated by [[utgenome.weaver.core.memory.MemoryAllocator#allocate]]
   * @param addr the address returned by  [[utgenome.weaver.core.memory.MemoryAllocator#allocate]]
   */
  def release(addr:Long) : Unit
}

object UnsafeUtil {

  val unsafe = {
    val f = classOf[Unsafe].getDeclaredField("theUnsafe")
    f.setAccessible(true)
    f.get(null).asInstanceOf[Unsafe]
  }

}



class UnsafeAllocator extends MemoryAllocator {


  /**
   * Allocate a memory of the specified byte length. The allocated memory must be released via [[utgenome.weaver.core.memory.MemoryAllocator# r e l e a s e]]
   * as in malloc() in C/C++.
   * @param size byte length of the memory
   * @return adress of the allocated mmoery.
   */
  def allocate(size: Long): Long = {
    UnsafeUtil.unsafe.allocateMemory(size)
  }

  /**
   * Release the memory allocated by [[utgenome.weaver.core.memory.MemoryAllocator# a l l o c a t e]]
   * @param addr [[utgenome.weaver.core.memory.MemoryAllocator#allocate]]
   */
  def release(addr: Long) {
    UnsafeUtil.unsafe.freeMemory(addr)
  }

}

class JNumaAllocator extends MemoryAllocator {

  import xerial.jnuma.Numa

  private val metaInfoSpace = 8L

  /**
   * Allocate a memory of the specified byte length. The allocated memory must be released via [[utgenome.weaver.core.memory.MemoryAllocator# r e l e a s e]]
   * as in malloc() in C/C++.
   * @param size byte length of the memory
   * @return adress of the allocated mmoery.
   */
  def allocate(size: Long): Long = {
    val s = metaInfoSpace + size
    val addr = Numa.allocMemory(s)
    // Write the buffer length as meta info
    UnsafeUtil.unsafe.putLong(addr, s)
    // Return the address after the meta info
    addr + metaInfoSpace
  }

  /**
   * Release the memory allocated by [[utgenome.weaver.core.memory.MemoryAllocator# a l l o c a t e]]
   * @param addr the address returned by  [[utgenome.weaver.core.memory.MemoryAllocator# a l l o c a t e]]
   */
  def release(addr: Long) {
    val start = addr - metaInfoSpace
    val size = UnsafeUtil.unsafe.getLong(start)
    Numa.free(start,size)
  }
}