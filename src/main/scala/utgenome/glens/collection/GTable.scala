package utgenome.glens.collection

import collection.mutable

//--------------------------------------
//
// GTable.scala
// Since: 2012/08/24 5:34 PM
//
//--------------------------------------




/**
 * list of GIntervals
 *
 * @author leo
 */
class GTable[A <: GenomicInterval[A]](implicit iv:IntervalType[A, Int]) {
  
  private val table = mutable.Map[String, PrioritySearchTree[A]]()

  def +=(e:A) : this.type = {
    val p = table.getOrElseUpdate(e.chr, PrioritySearchTree.empty[A](iv))
    table.update(e.chr, p + e)
    this
  }

  def clear = table.clear


  /**
   * Retrieve intervals intersecting with the given range and the same strand.
   * @param range
   * @return
   */
  def intersectWith(range:A) : TraversableOnce[A] = {
    table.get(range.chr) map { p =>
      p.intersectWith(range).filter { _.strand == range.strand  }
    } getOrElse Iterable.empty[A]
  } 

  def following[B <: GLocus](locus:B) : TraversableOnce[A] = {
    table.get(locus.chr) map { p =>
      p.range(Some(locus.start), None)
    } getOrElse Iterable.empty[A]
  }

}