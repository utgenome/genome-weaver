//--------------------------------------
//
// LArray.scala
// Since: 2013/03/13 13:40
//
//--------------------------------------

package utgenome.weaver.core.array

import utgenome.weaver.core.memory.{UnsafeUtil, MemoryAllocator}

/**
 * Large Array (LArray) interface. The differences from Array[T] includes:
 *
 *  - LArray accepts Long type indexes, so it is possible to create arrays more than 2GB entries, a limitation of Array[T].
 *  - The memory of LArray[T] resides outside of the normal garbage-collected JVM heap. So the user must release the memory via [[utgenome.weaver.core.array.LArray#free]].
 *  - LArray elements are not initialized, so explicit initialization is needed
 *  -
 * @tparam T
 */
trait LArray[T] {

  /**
   * Size of this array
   * @return size of this array
   */
  def size : Long

  /**
   * Retrieve an element
   * @param i index
   * @return the element value
   */
  def apply(i:Long) : T

  /**
   * Update an element
   * @param i index to be updated
   * @param v value to set
   * @return the value
   */
  def update(i:Long, v:T) : T

  /**
   * Release the memory of LArray. After calling this method, the results of calling the other methods becomes undefined or might cause JVM crash.
   */
  def free : Unit

}

/**
 * @author Taro L. Saito
 */
object LArray {

  object EmptyArray extends LArray[Nothing] {
    def size: Long = 0
    def apply(i: Long): Nothing = { sys.error("not allowed") }
    def update(i: Long, v: Nothing): Nothing = { sys.error("not allowed")}
    def free { /* do nothing */ }
  }

  def empty = EmptyArray
  def apply() = EmptyArray

  def apply(first:Int, elems:Int*) : LArray[Int] = {
    // elems: Int* => Seq[Int]
    val size = 1 + elems.size
    // TODO use strategy pattern to switch implementations
//    val arr = if(size > Int.MaxValue) // always false
//      new LIntArray(size)
//    else
//      new LIntArraySimple(size)
    val arr = new LIntArray(size)
    // Populate the array elements
    arr(0) = first
    for((e, i) <- elems.zipWithIndex) {
      arr(i+1) = e
    }
    arr
  }


}



class LIntArraySimple(val size:Long) extends LArray[Int] {
  private def boundaryCheck(i:Long) {
    if(i > Int.MaxValue)
      sys.error(f"index must be smaller than ${Int.MaxValue}%,d")
  }
  private val arr = {
    new Array[Int](size.toInt)
  }

  def apply(i: Long): Int = {
    boundaryCheck(i)
    arr.apply(i.toInt)
  }

  // a(i) = a(j) = 1
  def update(i: Long, v: Int) : Int = {
    boundaryCheck(i)
    arr.update(i.toInt, v)
    v
  }

  def free {
    // do nothing
  }
}

/**
 * LArray of Int type
 * @param size  the size of array
 * @param address memory address
 * @param mem memory allocator
 */
class LIntArray(val size:Long, address:Long)(implicit mem:MemoryAllocator) extends LArray[Int] {

  def this(size:Long)(implicit mem:MemoryAllocator) = this(size, mem.allocate(size << 2))

  private val unsafe : sun.misc.Unsafe = UnsafeUtil.unsafe

  def apply(i: Long): Int = {
    unsafe.getInt(address + (i << 2))
  }

  // a(i) = a(j) = 1
  def update(i: Long, v: Int) : Int = {
    unsafe.putInt(address + (i << 2), v)
    v
  }
  def free {
    mem.release(address)
  }

}

