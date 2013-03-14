//--------------------------------------
//
// MemoryAllocator.scala
// Since: 2013/03/14 9:26
//
//--------------------------------------

package utgenome.weaver.core.memory

import sun.misc.Unsafe
import xerial.core.log.Logger

object MemoryAllocator {

  /**
   * Provides default memory allocator
   */
  implicit val defaultAllocator : MemoryAllocator = new UnsafeAllocator

}


/**
 * Memory allocator interface
 *
 * @author Taro L. Saito
 */
trait MemoryAllocator extends Logger {

  private val allocatedMemoryAddr = collection.mutable.Set[Long]()

  // Register a shutdown hook to deallocate memory regions
  Runtime.getRuntime.addShutdownHook(new Thread(new Runnable {
    def run() {
      trace("Releasing allocated memory regions")
      synchronized {
        val addrSet = Set() ++ allocatedMemoryAddr // take a copy of the set
        for(addr <- addrSet) {
          warn(f"Found unreleased address:$addr%x")
          release(addr)
        }
      }
    }
  }))

  protected def allocateInternal(size:Long) : Long
  protected def releaseInternal(addr:Long) : Unit

  /**
   * Allocate a memory of the specified byte length. The allocated memory must be released via [[utgenome.weaver.core.memory.MemoryAllocator#release]]
   * as in malloc() in C/C++.
   * @param size byte length of the memory
   * @return adress of the allocated mmoery.
   */
  def allocate(size:Long) : Long = {
    synchronized {
      val addr = allocateInternal(size)
      allocatedMemoryAddr += addr
      addr
    }
  }

  /**
   * Release the memory allocated by [[utgenome.weaver.core.memory.MemoryAllocator#allocate]]
   * @param addr the address returned by  [[utgenome.weaver.core.memory.MemoryAllocator#allocate]]
   */
  def release(addr:Long) : Unit = {
    synchronized {
      if(addr != 0 && allocatedMemoryAddr.contains(addr)) {
        releaseInternal(addr)
        allocatedMemoryAddr -= addr
      }
    }
  }
}

object UnsafeUtil {

  val unsafe = {
    val f = classOf[Unsafe].getDeclaredField("theUnsafe")
    f.setAccessible(true)
    f.get(null).asInstanceOf[Unsafe]
  }

}


/**
 * Allocate memory using [[sun.misc.Unsafe]]. OpenJDK (and probably Oracle JDK) implements allocateMemory and freeMemory functions using malloc() and free() in C.
 */
class UnsafeAllocator extends MemoryAllocator with Logger {

  protected def allocateInternal(size: Long): Long = UnsafeUtil.unsafe.allocateMemory(size)
  protected def releaseInternal(addr: Long) = UnsafeUtil.unsafe.freeMemory(addr)
}

/**
 * Allocate memory using Numa API
 */
class NumaAllocator extends MemoryAllocator {

  import xerial.jnuma.Numa

  private val metaInfoSpace = 8L

  def allocateInternal(size: Long): Long = {
    val s = metaInfoSpace + size
    val addr = Numa.allocMemory(s)
    // Write the buffer length as meta info
    UnsafeUtil.unsafe.putLong(addr, s)
    // Return the address after the meta info
    addr + metaInfoSpace
  }

  def releaseInternal(addr: Long) {
    val start = addr - metaInfoSpace
    val size = UnsafeUtil.unsafe.getLong(start)
    Numa.free(start,size)
  }
}