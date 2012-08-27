package utgenome.glens.collection

import collection.mutable
import utgenome.glens.BEDGene
import java.io.File

//--------------------------------------
//
// GTable.scala
// Since: 2012/08/24 5:34 PM
//
//--------------------------------------


object GTable {
  def loadBED(bedFile:String) : GTable[BEDGene] = {
    val t = new GTable[BEDGene]
    for(bed <- BEDGene.parse(new File(bedFile))) {
      t += bed
    }
    t
  }
}

/**
 * A table of GIntervals
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

}