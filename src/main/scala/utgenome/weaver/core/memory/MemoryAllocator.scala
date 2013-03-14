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
  implicit val default : MemoryAllocator = new UnsafeAllocator

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
      releaseAll
    }
  }))

  /**
   * Release all memory addresses taken by this allocator.
   * Be careful in using this method, since all the memory addresses in LArray will be invalid.
   */
  def releaseAll {
    synchronized {
      val addrSet = Set() ++ allocatedMemoryAddr // take a copy of the set
      if(!addrSet.isEmpty)
        trace("Releasing allocated memory regions")
      for(addr <- addrSet) {
        warn(f"Found unreleased address:$addr%x")
        release(addr)
      }
    }
  }

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
      trace(f"allocated memory:$addr%X")
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
        trace(f"release memory:$addr%X")
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

  import UnsafeUtil.unsafe

  def newIntArray(size:Int) : Array[Int] = {
    // Create an empty array
    val arr = new Array[Int](0)
    //val addrSize = unsafe.addressSize
    val offset = unsafe.arrayBaseOffset(classOf[Array[Int]]).toLong
    trace(s"array offset: $offset")
    val addr = allocate(size)
    // write array size
    unsafe.putInt(arr, 12L, size)
    val prevAddr = unsafe.getLong(arr, offset)
    debug(f"prev addr: $prevAddr%X")
    // write memory location
    unsafe.putLong(arr, offset, addr)
    arr
  }


  protected def allocateInternal(size: Long): Long = unsafe.allocateMemory(size)
  protected def releaseInternal(addr: Long) = unsafe.freeMemory(addr)
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