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
trait GenomicInterval[Repr <: GenomicInterval[Repr]] extends GenInterval[Repr, Int]  { this : Repr =>

  val chr : String
  val strand : Strand

  def inSameChr[A <: Repr](other: A): Boolean = this.chr == other.chr

  def checkChr[A <: Repr, B](other: A, success: => B, fail: => B): B = {
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

  override def intersectWith[A <: Repr](other: A): Boolean = {
    checkChr(other, super.intersectWith(other), false)
  }

  override def contains[A <: Repr](other: A): Boolean = {
    checkChr(other, super.intersectWith(other), false)
  }

  override def intersection[A <: Repr](other: A): Option[Repr] = {
    checkChr(other, super.intersection(other), None)
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

}


/**
 * Representing a range in genome sequences with chr, start, end and strand values
 * @author leo
 */
class GInterval(val chr: String, val start: Int, val end: Int, val strand: Strand)
  extends GenomicInterval[GInterval] with Eq {
  override def toString = "%s:[%d, %d):%s".format(chr, start, end, strand)

  protected def intervalType = null
}


