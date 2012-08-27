//--------------------------------------
//
// GInterval.scala
// Since: 2012/08/27 1:54 PM
//
//--------------------------------------

package utgenome.glens.collection

import utgenome.glens.{Reverse, Forward, Strand}
import xerial.core.lens.Eq


/**
 * Common trait for representing intervals in genome sequences with chr and strand information
 */
trait GenomicInterval[Repr <: GenomicInterval[Repr]] { this : Repr =>
  protected def intervalType : IntervalType[Repr, Int]

  val chr : String
  val strand : Strand

  override def toString = "%d:%d".format(intervalType.start(this), intervalType.end(this))
  def size : Int = intervalType.ord.diff(intervalType.end(this), intervalType.start(this))

  def inSameChr[A <: GenomicInterval[_]](other: A): Boolean = this.chr == other.chr

  def checkChr[A <: GenomicInterval[_], Ret](other: A, success: => Ret, fail: => Ret): Ret = {
    if (inSameChr(other))
      success
    else
      fail
  }

  def fivePrimeEnd: GLocus = strand match {
    case Forward => new GLocus(chr, intervalType.start(this), strand)
    case Reverse => new GLocus(chr, intervalType.end(this), strand)
  }

  def threePrimeEnd: GLocus = strand match {
    case Forward => new GLocus(chr, intervalType.end(this), strand)
    case Reverse => new GLocus(chr, intervalType.start(this), strand)
  }

  def intersectWith[A <: GenomicInterval[_]](other: A)(implicit iv:IntervalType[A, Int]): Boolean = {
    checkChr(other, intervalType.intersect(this, other), false)
  }

  def contains[A <: GenomicInterval[_]](other: A)(implicit iv:IntervalType[A, Int]): Boolean = {
    checkChr(other, intervalType.contain(this, other), false)
  }

  def intersection[A <: GenomicInterval[_]](other: A)(implicit iv:IntervalType[A, Int]): Option[Repr] = {
    checkChr(other, intervalType.intersection(this, other), None)
  }

}

object GInterval {

  implicit object GIntervalType extends IntervalType[GInterval, Int] {

    def start(a: GInterval) = a.start
    def end(a: GInterval) = a.end

    def newInterval(base:GInterval, newStart:Int, newEnd:Int) = new GInterval(base.chr, newStart, newEnd, base.strand)

    /**
     * Ordering function of type V values
     * @return
     */
    def ord = OrderingOpt.IntOrd
  }

  def apply(chr: String, start: Int, end: Int, strand: Strand) = new GInterval(chr, start, end, strand)
  def apply(chr: String, start: Int, end: Int) = new GInterval(chr, start, end, Forward)

}


/**
 * Representing a range in genome sequences with chr, start, end and strand values
 * @author leo
 */
class GInterval(val chr: String, val start: Int, val end: Int, val strand: Strand)
  extends GenomicInterval[GInterval] with Eq {
  override def toString = "%s:[%d, %d):%s".format(chr, start, end, strand)

  protected def intervalType = GInterval.GIntervalType
}


