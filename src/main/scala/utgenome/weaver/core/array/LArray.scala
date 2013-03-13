//--------------------------------------
//
// LArray.scala
// Since: 2013/03/13 13:40
//
//--------------------------------------

package utgenome.weaver.core.array

import java.nio.ByteBuffer
import sun.misc.Unsafe
import sun.nio.ch.DirectBuffer

/**
 * @author Taro L. Saito
 */
object LArray {


  object EmptyArray extends LArrayTrait[Nothing] {
    def size: Long = 0
    def apply(i: Long): Nothing = { sys.error("not allowed") }
    def update(i: Long, v: Nothing): Nothing = { sys.error("not allowed")}
  }

  def apply() = EmptyArray

  def apply(first:Int, elems:Int*) : LArrayTrait[Int] = {
    // Int => Seq[Int]
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


trait LArrayTrait[T] {

  def size : Long
  def apply(i:Long) : T
  // a(i) = a(j) = 1
  def update(i:Long, v:T) : T

}

class LIntArraySimple(val size:Long) extends LArrayTrait[Int] {
  private def boundaryCheck(i:Long) {
    if(i > Int.MaxValue)
      sys.error(f"index must be smaller than ${Int.MaxValue}%,d")
  }
  private val arr = {
    boundaryCheck(size)
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
}


class LIntArray(val size:Long) extends LArrayTrait[Int] {
  private def boundaryCheck(i:Long) {
    if(i > Int.MaxValue)
      sys.error(f"index must be smaller than ${Int.MaxValue}%,d")
  }
  private val unsafe : Unsafe = {
    val f = classOf[Unsafe].getDeclaredField("theUnsafe")
    f.setAccessible(true)
    f.get(null).asInstanceOf[Unsafe]
  }

  private val address = {
    // TODO use JNuma
    val b = ByteBuffer.allocateDirect(size.toInt * 4).asInstanceOf[DirectBuffer]
    b.address()
  }

//  private val arr = {
//    //new Array[Byte](size.toInt * 4)
//  }


  def apply(i: Long): Int = {
    //unsafe.getInt(arr, i * 4)
    unsafe.getInt(address + i * 4)
  }

  // a(i) = a(j) = 1
  def update(i: Long, v: Int) : Int = {
    //unsafe.putInt(arr, i * 4, v)
    unsafe.putInt(address + i * 4, v)
    v
  }
}

