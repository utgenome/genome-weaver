//--------------------------------------
//
// OrderingOpt.scala
// Since: 2012/08/27 2:40 PM
//
//--------------------------------------

package utgenome.weaver.core

/**
 * <p>
 *   A trait for representing ordering of values. This class is optimized for Int and Long values
 * </p>
 * @tparam T
 */
trait OrderingOpt[@specialized(Int, Long) T] {

  def diff(x:T, y:T) : T

  def max(x:T, y:T) : T = if(lteq(x, y)) y else x
  def min(x:T, y:T) : T = if(lteq(x, y)) x else y

  def compare(x:T, y:T) : Int

  /** Returns <code>true</code> iff <code>x</code> comes before
   *  <code>y</code> in the ordering.
   */
  def lteq(x: T, y: T): Boolean

  /** Returns <code>true</code> iff <code>y</code> comes before
   *  <code>x</code> in the ordering.
   */
  def gteq(x: T, y: T): Boolean = lteq(y, x)

  /** Returns <code>true</code> iff <code>x</code> comes before
   *  <code>y</code> in the ordering and is not the same as <code>y</code>.
   */
  def lt(x: T, y: T): Boolean = lteq(x, y) && !equiv(x, y)

  /** Returns <code>true</code> iff <code>y</code> comes before
   *  <code>x</code> in the ordering and is not the same as <code>x</code>.
   */
  def gt(x: T, y: T): Boolean = gteq(x, y) && !equiv(x, y)

  /** Returns <code>true</code> iff <code>x</code> is equivalent to
   *  <code>y</code> in the ordering.
   */
  def equiv(x: T, y: T): Boolean = lteq(x,y) && lteq(y,x)
}

object OrderingOpt {
  object IntOrd extends OrderingOpt[Int] {
    def compare(x:Int, y:Int) : Int = x - y
    /**Returns <code>true</code> iff <code>x</code> comes before
     * <code>y</code> in the ordering.
     */
    def lteq(x: Int, y: Int) = x <= y
    override def lt(x: Int, y: Int): Boolean = x < y
    override def equiv(x: Int, y: Int): Boolean = x == y

    def diff(x: Int, y: Int) = x - y
  }

  object LongOrd extends OrderingOpt[Long] {
    def compare(x:Long, y:Long) : Int = {
      val diff = x - y
      if(diff < 0)
        -1
      else if(diff == 0)
        0
      else
        1
    }
    /**Returns <code>true</code> iff <code>x</code> comes before
     * <code>y</code> in the ordering.
     */
    def lteq(x: Long, y: Long) = x <= y
    override def lt(x: Long, y: Long): Boolean = x < y
    override def equiv(x: Long, y: Long): Boolean = x == y

    def diff(x: Long, y: Long) = x - y
  }

}
