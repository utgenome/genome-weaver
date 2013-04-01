//--------------------------------------
//
// LArray.scala
// Since: 2013/03/13 13:40
//
//--------------------------------------

package utgenome.weaver.core.array

import utgenome.weaver.core.memory.{UnsafeUtil, MemoryAllocator}
import scala.reflect.runtime.{universe => ru}
import ru._
import utgenome.weaver.core.memory.UnsafeUtil._
import xerial.core.log.Logger
import collection.GenIterable

/**
 * Large Array (LArray) interface. The differences from Array[A] includes:
 *
 * - LArray accepts Long type indexes, so it is possible to create arrays more than 2GB entries, a limitation of Array[A].
 * - The memory of LArray[A] resides outside of the normal garbage-collected JVM heap. So the user must release the memory via [[utgenome.weaver.core.array.LArray#free]].
 * - LArray elements are not initialized, so explicit initialization is needed
 * -
 * @tparam A
 */
trait LArray[A] extends LArrayOps[A] with LIterable[A] {

  /**
   * Size of this array
   * @return size of this array
   */
  def size: Long

  /**
   * byte length of this array
   * @return
   */
  def byteLength: Long

  /**
   * Retrieve an element
   * @param i index
   * @return the element value
   */
  def apply(i: Long): A

  /**
   * Update an element
   * @param i index to be updated
   * @param v value to set
   * @return the value
   */
  def update(i: Long, v: A): A

  /**
   * Release the memory of LArray. After calling this method, the results of calling the other methods becomes undefined or might cause JVM crash.
   */
  def free: Unit


}


/**
 * @author Taro L. Saito
 */
object LArray {

  object EmptyArray
    extends LArray[Nothing]
    with LIterable[Nothing]
  {
    def size: Long = 0L
    def byteLength = 0L

    def apply(i: Long): Nothing = {
      sys.error("not allowed")
    }

    def update(i: Long, v: Nothing): Nothing = {
      sys.error("not allowed")
    }

    def free {
      /* do nothing */
    }
    def write(srcOffset: Long, dest: Array[Byte], destOffset: Int, length: Int): Int = 0

    /**
     * Read the contents from a given source buffer
     * @param src
     * @param srcOffset
     * @param destOffset
     * @param length
     */
    def read(src: Array[Byte], srcOffset: Int, destOffset: Long, length: Int) = 0

  }

  def empty = EmptyArray

  def apply() = EmptyArray

  /**
   * Creates an LArray with given elements.
   *
   * @param xs the elements to put in the array
   * @return an array containing all elements from xs.
   */
  def apply[T: TypeTag](xs: T*): LArray[T] = {
    val size = xs.size
    val t = typeOf[T]
    val arr : LArray[T] = t match {
      case t if t =:= typeOf[Int] => new LIntArray(size).asInstanceOf[LArray[T]]
      case t if t =:= typeOf[Byte] => new LByteArray(size).asInstanceOf[LArray[T]]
      case _ => sys.error(s"Unsupported type LArray[${t}}]")
    }
    var i = 0
    for(x <- xs) { arr(i) = x; i += 1 }
    arr
  }


  def apply(first: Int, elems: Int*): LArray[Int] = {
    // elems: Int* => Seq[Int]
    val size = 1 + elems.size
    val arr = new LIntArray(size)
    // Populate the array elements
    arr(0) = first
    for ((e, i) <- elems.zipWithIndex) {
      arr(i + 1) = e
    }
    arr
  }

  def apply(first: Byte, elems: Byte*): LArray[Byte] = {
    val size = 1 + elems.size
    val arr = new LByteArray(size)
    arr(0) = first
    for ((e, i) <- elems.zipWithIndex) {
      arr(i + 1) = e
    }
    arr
  }

}

/**
 * Wrapping Array[Int] to support Long-type indexes
 * @param size
 */
class LIntArraySimple(val size: Long) extends LArray[Int] {

  def byteLength = size * 4

  private def boundaryCheck(i: Long) {
    if (i > Int.MaxValue)
      sys.error(f"index must be smaller than ${Int.MaxValue}%,d")
  }

  private val arr = {
    new Array[Int](size.toInt)
  }

  def apply(i: Long): Int = {
    //boundaryCheck(i)
    arr.apply(i.toInt)
  }

  // a(i) = a(j) = 1
  def update(i: Long, v: Int): Int = {
    //boundaryCheck(i)
    arr.update(i.toInt, v)
    v
  }

  def free {
    // do nothing
  }

  /**
   * Write the contents of this array to the destination buffer
   * @param srcOffset byte offset
   * @param dest destination array
   * @param destOffset offset in the destination array
   * @param length the byte length to write
   * @return byte length to write
   */
  def write(srcOffset: Long, dest: Array[Byte], destOffset: Int, length: Int): Int = {
    System.arraycopy(arr, srcOffset.toInt, dest, destOffset, length)
    length
  }

  /**
   * Read the contents from a given source buffer
   * @param src
   * @param srcOffset
   * @param destOffset
   * @param length
   */
  def read(src: Array[Byte], srcOffset: Int, destOffset: Long, length: Int) = {
    System.arraycopy(src, srcOffset, arr, destOffset.toInt, length)
    length
  }
}


/**
 * Emulate large arrays using two-diemensional matrix of Int. Array[Int](page index)(offset in page)
 * @param size
 */
class MatrixBasedLIntArray(val size:Long) extends LArray[Int] {

  def byteLength = size * 4


  private val maskLen : Int = 24
  private val B : Int = 1 << maskLen // block size
  private val mask : Long = ~(~0L << maskLen)

  @inline private def index(i:Long) : Int = (i >>> maskLen).toInt
  @inline private def offset(i:Long) : Int = (i & mask).toInt

  private val numBlocks = ((size + (B - 1L))/ B).toInt
  private val arr = Array.ofDim[Int](numBlocks, B)

  /**
   * Retrieve an element
   * @param i index
   * @return the element value
   */
  def apply(i: Long) = arr(index(i))(offset(i))

  /**
   * Update an element
   * @param i index to be updated
   * @param v value to set
   * @return the value
   */
  def update(i: Long, v: Int) = {
    arr(index(i))(offset(i)) = v
    v
  }

  /**
   * Release the memory of LArray. After calling this method, the results of calling the other methods becomes undefined or might cause JVM crash.
   */
  def free {}

  /**
   * Write the contents of this array to the destination buffer
   * @param srcOffset byte offset
   * @param dest destination array
   * @param destOffset offset in the destination array
   * @param length the byte length to write
   * @return byte length to write
   */
  def write(srcOffset: Long, dest: Array[Byte], destOffset: Int, length: Int): Int = {
    // TODO
    0
  }

  /**
   * Read the contents from a given source buffer
   * @param src
   * @param srcOffset
   * @param destOffset
   * @param length
   */
  def read(src: Array[Byte], srcOffset: Int, destOffset: Long, length: Int) = {
    // TODO
    0
  }
}


private[array] trait UnsafeArray[T] extends Logger { self: LArray[T] =>

  def address: Long

  /**
   * Write the contents of this array to the destination buffer
   * @param srcOffset byte offset
   * @param dest destination array
   * @param destOffset offset in the destination array
   * @param length the byte length to write
   * @return written byte length
   */
  def write(srcOffset: Long, dest: Array[Byte], destOffset: Int, length: Int): Int = {
    val writeLen = math.min(dest.length - destOffset, math.min(length, byteLength - srcOffset))
    // Retrieve destination array address
    // TODO Use JNI to get Collect Address
    val destAddr = UnsafeUtil.getObjectAddr(dest) + UnsafeUtil.byteArrayOffset
    trace(f"dest addr:$destAddr%x")
    unsafe.copyMemory(address + srcOffset, destAddr + destOffset, writeLen)
    writeLen.toInt
  }

  def read(src:Array[Byte], srcOffset:Int, destOffset:Long, length:Int) : Int = {
    val srcAddr = getObjectAddr(src) + UnsafeUtil.byteArrayOffset
    val readLen = math.min(src.length-srcOffset, math.min(byteLength - destOffset, length))
    debug(s"read len: $readLen")
    unsafe.copyMemory(srcAddr, address + destOffset, readLen)
    readLen.toInt
  }

}

/**
 * LArray of Int type
 * @param size  the size of array
 * @param address memory address
 * @param mem memory allocator
 */
class LIntArray(val size: Long, val address: Long)(implicit mem: MemoryAllocator)
  extends LArray[Int]
  with UnsafeArray[Int]
{
  def this(size: Long)(implicit mem: MemoryAllocator) = this(size, mem.allocate(size << 2))

  def byteLength = size * 4

  import UnsafeUtil.unsafe

  def apply(i: Long): Int = {
    unsafe.getInt(address + (i << 2))
  }

  // a(i) = a(j) = 1
  def update(i: Long, v: Int): Int = {
    unsafe.putInt(address + (i << 2), v)
    v
  }

  def free {
    mem.release(address)
  }
}

/**
 * LArray of Byte type
 * @param size
 * @param address
 * @param mem
 */
class LByteArray(val size: Long, val address: Long)(implicit mem: MemoryAllocator)
  extends LArray[Byte]
  with UnsafeArray[Byte]
{
  self =>

  def this(size: Long)(implicit mem: MemoryAllocator) = this(size, mem.allocate(size))

  def byteLength = size

  /**
   * Retrieve an element
   * @param i index
   * @return the element value
   */
  def apply(i: Long): Byte = {
    UnsafeUtil.unsafe.getByte(address + i)
  }

  /**
   * Update an element
   * @param i index to be updated
   * @param v value to set
   * @return the value
   */
  def update(i: Long, v: Byte): Byte = {
    UnsafeUtil.unsafe.putByte(address + i, v)
    v
  }

  /**
   * Release the memory of LArray. After calling this method, the results of calling the behavior of the other methods becomes undefined or might cause JVM crash.
   */
  def free {
    mem.release(address)
  }

  def sort {

    def sort(left:Long, right:Long) {
      val NUM_BYTE_VALUES = 256
      // counting sort
      val count: Array[Int] = new Array[Int](NUM_BYTE_VALUES)

      {
        var i : Long = left - 1
        while ({ i += 1; i <= right}) {
          count(self(i) - Byte.MinValue) += 1
        }
      }

      {
        var i = NUM_BYTE_VALUES
        var k : Long = right + 1
        while (k > left) {
          while({ i -= 1; count(i) == 0} ) {}
          val value: Byte = (i + Byte.MinValue).toByte
          var s = count(i)
          do {
            k -= 1
            self(k) = value
          } while (({s -= 1; s}) > 0)
        }
      }
    }

    sort(0L, size-1L)
  }


}