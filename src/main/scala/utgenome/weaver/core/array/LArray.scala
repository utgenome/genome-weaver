//--------------------------------------
//
// LArray.scala
// Since: 2013/03/13 13:40
//
//--------------------------------------

package utgenome.weaver.core.array

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

  def apply(first:Int, elems:Int*) : LIntArray = {
    // Int => Seq[Int]
    new LIntArray(Array(first, elems:_*))
  }

}


trait LArrayTrait[T] {

  def size : Long
  def apply(i:Long) : T
  // a(i) = a(j) = 1
  def update(i:Long, v:T) : T

}

class LIntArray(arr:Array[Int]) extends LArrayTrait[Int] {
  def size: Long = arr.size

  private def boundaryCheck(i:Long) {
    if(i > Int.MaxValue)
      sys.error(f"index must be smaller than ${Int.MaxValue}%,d")
  }

  def apply(i: Long): Int = {
    boundaryCheck(i)
    arr.apply(i.toInt)
  }

  // a(i) = a(j) = 1
  def update(i: Long, v: Int): Int = {
    boundaryCheck(i)
    arr.update(i.toInt, v)
    v
  }
}




