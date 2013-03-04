//--------------------------------------
//
// GRange.scala
// Since: 2012/08/27 1:54 PM
//
//--------------------------------------

package utgenome.weaver.core

import xerial.lens.Eq
import xerial.core.log.Logger
import reflect.ClassTag

/**
 * A type class for mapping A to GLocus
 * @tparam A
 */
trait GLocusType[A] {
  def start(a:A) : Int
  def chr(a:A): String
  def strand(a:A): Strand
}

/**
 * Locus in a genome sequence with chr and strand information
 */
trait GenomicLocus[A] extends Eq { this : A =>

  @inline protected def ev : GLocusType[A]

  /**
   *
   * @param width
   * @return
   */
  def around(width: Int): GInterval = newRange(ev.start(this) - width, ev.start(this) + width)
  def around(upstreamLength: Int, downstreamLength: Int) = ev.strand(this) match {
    case Forward => newRange(ev.start(this) - upstreamLength, ev.start(this) + downstreamLength)
    case Reverse => newRange(ev.start(this) - downstreamLength, ev.start(this) + upstreamLength)
  }
  def upstream(length: Int): GInterval = ev.strand(this) match {
    case Forward => newRange(ev.start(this) - length, ev.start(this))
    case Reverse => newRange(ev.start(this), ev.start(this) + length)
  }
  def downstream(length: Int): GInterval = ev.strand(this) match {
    case Forward => newRange(ev.start(this), ev.start(this) + length)
    case Reverse => newRange(ev.start(this) - length, ev.start(this))
  }

  def toRange: GInterval = GInterval(ev.chr(this), ev.start(this), ev.start(this), ev.strand(this))
  def newRange(newStart: Int, newEnd: Int): GInterval = GInterval(ev.chr(this), newStart, newEnd, ev.strand(this))

  def -[B](other:B)(implicit t:GLocusType[B]) :Int = {
    ev.start(this) - t.start(other)
  }

  def +[B](other:B)(implicit t:GLocusType[B]) :Int = {
    ev.start(this) + t.start(other)
  }


  def distanceTo[B](other:B)(implicit t:GLocusType[B]) :Int = {
    t.start(other) - ev.start(this)
  }
}


object GLocus {


  implicit object GLocusAsGInterval extends GIntervalType[GLocus] {
    def start(a:GLocus) : Int = a.start
    def end(a:GLocus) : Int = a.start
    def chr(a:GLocus): String  = a.chr
    def strand(a:GLocus): Strand = a.strand
  }

  private[this] val ivTable = collection.mutable.Map[Class[_], GLocusType[_]]()

  /**
   *
   * @tparam A
   * @return
   */
  implicit def createTypeClass[A <: GLocus](implicit t:ClassTag[A]) : GLocusType[A] = {
    ivTable.getOrElseUpdate(t.runtimeClass,
      new GLocusType[A] {
        def start(a:A) : Int = a.start
        def chr(a:A): String = a.chr
        def strand(a:A): Strand = a.strand
      }
    ).asInstanceOf[GLocusType[A]]
  }

  implicit object GLocusOrdering extends Ordering[GLocus] {
    def compare(a: GLocus, b:GLocus) : Int = {
      // compare chr and start (no comparison for strand values)
      var diff = a.chr.compareTo(b.chr)
      if(diff == 0)
        diff = a.start - b.start
      diff
    }
  }

  def apply(chr:String, start:Int, strand:Strand)= new GLocus(chr, start, strand)
  def apply(chr:String, start:Int)= new GLocus(chr, start, Forward)
}


/**
 * Locus in a genome sequence
 * @param chr
 * @param start
 * @param strand
 */
class GLocus(val chr: String, val start: Int, val strand: Strand)
  extends GenomicLocus[GLocus] {
  override def toString = "%s:%d:%s".format(chr, start, strand)

  protected def ev = GLocus.createTypeClass[GLocus]

  def move(newStart: Int) = new GLocus(chr, newStart, strand)

}

/**
 * Type class for mapping A to GInterval
 * @tparam A  actual class
 */
trait GIntervalType[A] extends IntervalType[A] with GLocusType[A] {
  def end(a:A) : Int
}

object GenomicInterval {

}

/**
 * Common trait for representing intervals in genome sequences with chr and strand information
 */
trait GenomicInterval[A] extends Eq { this: A =>

  /**
   * Provide an evidence that A is a GIntervalType
   * @return
   */
  @inline protected def ev : GIntervalType[A]

  override def toString = "%d:%d".format(ev.start(this), ev.end(this))
  def length : Int = ev.end(this) - ev.start(this)

  def inSameChr[B](other: B)(implicit t : GIntervalType[B]): Boolean = ev.chr(this) == t.chr(other)

  def checkChr[B, Ret](other: B, success: => Ret, fail: => Ret)(implicit t : GIntervalType[B]): Ret = {
    if (inSameChr(other))
      success
    else
      fail
  }

  def fivePrimeEnd: GLocus = ev.strand(this) match {
    case Forward => new GLocus(ev.chr(this), ev.start(this), ev.strand(this))
    case Reverse => new GLocus(ev.chr(this), ev.end(this), ev.strand(this))
  }

  def threePrimeEnd: GLocus = ev.strand(this) match {
    case Forward => new GLocus(ev.chr(this), ev.end(this), ev.strand(this))
    case Reverse => new GLocus(ev.chr(this), ev.start(this), ev.strand(this))
  }

  def intersectWith[B](other: B)(implicit t:GIntervalType[B]): Boolean = {
    checkChr(other, ev.intersect(this, other)(t), false)
  }

  def contains[B](other: B)(implicit t:GIntervalType[B]): Boolean = {
    checkChr(other, ev.contain(this, other)(t), false)
  }

  def containsPoint(pos:GLocus) : Boolean = {
    checkChr(pos, ev.containPoint(this, pos.start), false)
  }

  def intersection[B](other: B)(implicit t:GIntervalType[B]): Option[GInterval] = {
    checkChr(other, ev.intersection(this, other)(t) map { g => GInterval(ev.chr(this), g.start, g.end, ev.strand(this)) }, None)
  }

}


object GInterval extends Logger {

  implicit class WrapAsGInterval[A <: GInterval](a:A)(implicit iv:GIntervalType[A]) {
    def chr = iv.chr(a)
    def start = iv.start(a)
    def end = iv.end(a)
    def strand = iv.strand(a)
  }


  private[this] val ivTable = collection.mutable.Map[Class[_], GIntervalType[_]]()

  /**
   *
   * @tparam A
   * @return
   */
  implicit def createTypeClass[A <: GInterval](implicit t:ClassTag[A]) : GIntervalType[A] = {
    ivTable.getOrElseUpdate(t.runtimeClass,
      new GIntervalType[A] {
        def start(a:A) : Int = a.start
        def end(a:A) : Int = a.end
        def chr(a:A): String = a.chr
        def strand(a:A): Strand = a.strand
      }
    ).asInstanceOf[GIntervalType[A]]
  }

  def apply(chr: String, start: Int, end: Int, strand: Strand) = new GInterval(chr, start, end, strand)
  def apply(chr: String, start: Int, end: Int) = new GInterval(chr, start, end, Forward)


  implicit object GIntervalOrdering extends Ordering[GInterval] {
    def compare(x: GInterval, y: GInterval) = {
      var diff = x.chr.compare(y.chr)
      if(diff == 0)
        diff = x.start - y.start
      if(diff == 0)
        diff = x.end - y.end
      diff
    }
  }

}


/**
 * Representing a range in genome sequences with chr, start, end and strand values
 * @author leo
 */
class GInterval(val chr: String, val start: Int, val end: Int, val strand: Strand)
  extends GenomicInterval[GInterval]  {
  override def toString = "%s:[%d, %d):%s".format(chr, start, end, strand)

  @inline protected def ev = GInterval.createTypeClass[GInterval]
}


