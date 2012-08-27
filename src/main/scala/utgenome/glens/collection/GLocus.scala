//--------------------------------------
//
// GLocus.scala
// Since: 2012/08/27 2:55 PM
//
//--------------------------------------

package utgenome.glens.collection

import xerial.core.lens.Eq
import utgenome.glens.{Reverse, Forward, Strand}


/**
 * Locus in a genome sequence with chr and strand information
 */
trait GenomicLocus[Repr, RangeRepr] extends Eq {
  val start: Int
  val chr : String
  val strand : Strand

  /**
   *
   * @param width
   * @return
   */
  def around(width: Int): RangeRepr = newRange(start - width, start + width)
  def around(upstreamLength: Int, downstreamLength: Int) = strand match {
    case Forward => newRange(start - upstreamLength, start + downstreamLength)
    case Reverse => newRange(start - downstreamLength, start + upstreamLength)
  }
  def upstream(length: Int): RangeRepr = strand match {
    case Forward => newRange(start - length, start)
    case Reverse => newRange(start, start + length)
  }
  def downstream(length: Int): RangeRepr = strand match {
    case Forward => newRange(start, start + length)
    case Reverse => newRange(start - length, start)
  }

  def toRange: RangeRepr

  def newRange(newStart: Int, newEnd: Int): RangeRepr

  def -[A <: GenomicLocus[_, _]](other:A) :Int = {
    this.start - other.start
  }

  def +[A <: GenomicLocus[_, _]](other:A) :Int = {
    this.start + other.start
  }


  def distanceTo[A <: GenomicLocus[_, _]](other:A) :Int = {
    other.start - this.start
  }
}


/**
 * Locus in a genome sequence
 * @param chr
 * @param start
 * @param strand
 */
case class GLocus(val chr: String, val start: Int, val strand: Strand)
  extends GenomicLocus[GLocus, GInterval] with Ordered[GLocus] {
  override def toString = "%s:%d:%s".format(chr, start, strand)
  def move(newStart: Int) = new GLocus(chr, newStart, strand)
  def newRange(newStart: Int, newEnd: Int) = new GInterval(chr, newStart, newEnd, strand)
  def toRange = new GInterval(chr, start, start, strand)

  def compare(other: GLocus) = {
    // compare chr and start (no comparison for strand values)
    var diff = chr.compareTo(other.chr)
    if(diff == 0)
      diff = start - other.start
    diff
  }

}
