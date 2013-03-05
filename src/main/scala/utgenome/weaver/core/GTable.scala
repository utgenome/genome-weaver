package utgenome.weaver.core

import java.io.File
import scala.collection.mutable

//--------------------------------------
//
// GTable.scala
// Since: 2012/08/24 5:34 PM
//
//--------------------------------------


object GTable {
  def loadBED(bedFile:String) : GTable[BEDGene] = {
    val t = new GTable[BEDGene]()
    for(bed <- BEDGene.parse(new File(bedFile))) {
      t += bed
    }
    t
  }

  def apply[A](input:Seq[A])(implicit iv:GIntervalType[A]) : GTable[A] = {
    val t = new GTable[A]
    input foreach { t += _ }
    t
  }

}

/**
 * A table of GIntervals
 *
 * @author leo
 */
class GTable[A]()(implicit iv:GIntervalType[A]) extends Traversable[A] {
  
  private val table = mutable.Map[String, PrioritySearchTree[A]]()


  def chrSet = table.keySet

  def apply(chr:String) = table.getOrElseUpdate(chr, PrioritySearchTree.empty[A](iv))

  override def size : Int = table.values.map{_.size}.sum

  def +=(e:A) : this.type = {
    val p = table.getOrElseUpdate(iv.chr(e), PrioritySearchTree.empty[A](iv))
    table.update(iv.chr(e), p + e)
    this
  }

  def clear = table.clear


  /**
   * Retrieve intervals intersecting with the given range and the same strand.
   * @param range
   * @return
   */
  def intersectWith[B](range:B)(implicit iv2:GIntervalType[B]) : TraversableOnce[A] = {
    table.get(iv2.chr(range)) map { p =>
      p.intersectWith(range)(iv2).filter { a => iv.strand(a) == iv2.strand(range)  }
    } getOrElse Iterable.empty[A]
  }

  def hasOverlap[B](range:B)(implicit iv2:GIntervalType[B]) : Boolean = {
    !intersectWith(range).isEmpty
  }

  def foreach[U](f: (A) => U) {
    table.values foreach { p => p.foreach(f(_)) }
  }
}