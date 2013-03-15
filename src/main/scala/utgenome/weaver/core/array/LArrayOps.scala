//--------------------------------------
//
// LArrayOps.scala
// Since: 2013/03/15 14:39
//
//--------------------------------------

package utgenome.weaver.core.array

/**
 * Operations for LArray
 *
 * @author Taro L. Saito
 */
trait LArrayOps[T] {
  def foreach[U](f: T => U) : Unit
  def forall(f: T => Boolean) : Boolean
  def mkString(sep:String) : String
  def sameElements(that:LArray[T]) : Boolean
  def zipWithIndex : Iterator[(T, Long)]

  /**
   * Write the contents of this array to the destination buffer
   * @param srcOffset byte offset
   * @param dest destination array
   * @param destOffset offset in the destination array
   * @param length the byte length to write
   * @return byte length to write
   */
  def write(srcOffset:Long, dest:Array[Byte], destOffset:Int, length:Int) : Int

  /**
   * Read the contents from a given source buffer
   * @param src
   * @param srcOffset
   * @param destOffset
   * @param length
   */
  def read(src:Array[Byte], srcOffset:Int, destOffset:Long, length:Int) : Int


}


/**
 * Default implemenation of LArrayOps
 * @tparam T
 */
trait LArrayOpsImpl[T] extends LArrayOps[T]
{
  self : LArray[T] =>

  private class LIterator extends Iterator[T] {
    private[array] var cursor = 0L
    def hasNext: Boolean = cursor < size

    def next(): T = {
      val v = self(cursor)
      cursor += 1
      v
    }
  }

  def iterator : Iterator[T] = new LIterator

  def forall(f:T => Boolean) = iterator.forall(f)

  def zipWithIndex = new Iterator[(T, Long)] {
    private val it = new LIterator
    def hasNext: Boolean = it.hasNext
    def next(): (T, Long) = {
      val c = it.cursor
      (it.next, c)
    }
  }

  def sameElements(that:LArray[T]) : Boolean = {
    if (self.size != that.size)
      return false
    var i = 0L
    while(i < self.size) {
      if(self(i) != that(i))
        return false
      i += 1
    }
    return true
  }

  def foreach[U](f: T => U) {
    var i = 0L
    while(i < size) {
      f(self(i))
      i+=1
    }
  }


  def mkString(sep:String) : String = {
    val b = new StringBuilder
    var i = 0L
    while(i < size) {
      if(i != 0)
        b append sep
      b append self(i)
      i+=1
    }
    b.result
  }


}
