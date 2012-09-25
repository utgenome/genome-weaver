//--------------------------------------
//
// GInterval.scala
// Since: 2012/08/27 1:54 PM
//
//--------------------------------------

package utgenome.glens.collection

import utgenome.glens.{Reverse, Forward, Strand}
import xerial.lens.Eq

object GenomicInterval {

  class GenomicIntervalOrdering[A <: GenomicInterval[_]](implicit iv : IntervalType[A]) extends Ordering[A] {
    def compare(x: A, y: A) = {
      var diff = x.chr.compare(y.chr)
      if(diff == 0)
        diff = iv.compareX(x, y)
      if(diff == 0)
        diff = iv.compareY(x, y)
      diff
    }
  }

}

trait InChromosome {
  val chr : String
}


/**
 * Common trait for representing intervals in genome sequences with chr and strand information
 */
trait GenomicInterval[Repr <: GenomicInterval[Repr]] extends InChromosome with Eq { this : Repr =>
  protected def intervalType : IntervalType[Repr]

  val start : Int
  val end : Int
  val strand : Strand

  override def toString = "%d:%d".format(intervalType.start(this), intervalType.end(this))
  def length : Int = intervalType.end(this) - intervalType.start(this)

  def inSameChr[A <: InChromosome](other: A): Boolean = this.chr == other.chr

  def checkChr[A <: InChromosome, Ret](other: A, success: => Ret, fail: => Ret): Ret = {
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

  def intersectWith[A <: GenomicInterval[_]](other: A)(implicit iv:IntervalType[A]): Boolean = {
    checkChr(other, intervalType.intersect(this, other), false)
  }

  def contains[A <: GenomicInterval[_]](other: A)(implicit iv:IntervalType[A]): Boolean = {
    checkChr(other, intervalType.contain(this, other), false)
  }

  def containsPoint(pos:GLocus) : Boolean = {
    checkChr(pos, intervalType.containPoint(this, pos.start), false)
  }

  def intersection[A <: GenomicInterval[_]](other: A)(implicit iv:IntervalType[A]): Option[GInterval] = {
    checkChr(other, intervalType.intersection(this, other) map { newGInterval(_) }, None)
  }

  def newGInterval(newRange:Interval) : GInterval = GInterval(chr, newRange.start, newRange.end, strand)

}

object GInterval {

  abstract class GIntervalTypeBase[A <: GInterval] extends IntervalType[A] {
    def start(a: A) = a.start
    def end(a: A) = a.end
  }


  implicit object GIntervalType extends GIntervalTypeBase[GInterval] {
    def newInterval(base:GInterval, newStart:Int, newEnd:Int) = new GInterval(base.chr, newStart, newEnd, base.strand)
  }

  def apply(chr: String, start: Int, end: Int, strand: Strand) = new GInterval(chr, start, end, strand)
  def apply(chr: String, start: Int, end: Int) = new GInterval(chr, start, end, Forward)

}


/**
 * Representing a range in genome sequences with chr, start, end and strand values
 * @author leo
 */
class GInterval(val chr: String, val start: Int, val end: Int, val strand: Strand)
  extends GenomicInterval[GInterval]  {
  override def toString = "%s:[%d, %d):%s".format(chr, start, end, strand)

  protected def intervalType = GInterval.GIntervalType
}


