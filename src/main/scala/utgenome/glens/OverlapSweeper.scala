//--------------------------------------
//
// RangeSweeper.scala
// Since: 2012/07/03 2:07 PM
//
//--------------------------------------

package utgenome.glens

import annotation.tailrec
import collection.{IntIntervalType}

/**
 * Sweeper of the genome range
 *
 * @author leo
 */
class OverlapSweeper[A](list:TraversableOnce[A])(implicit tc:IntIntervalType[A]) extends Iterator[Seq[A]] {

  private val it = list.toIterator
  private var nextOverlappedSet : Option[Seq[A]] = None
  private var sweepLine = 0

  private val endValueQueue = new scala.collection.mutable.PriorityQueue[A]()(new Ordering[A] {
    def compare(x: A, y: A) = {
      val diff = tc.end(y) - tc.end(x) // lower end value has high priority
      if(diff == 0)
        tc.start(y) - tc.start(x)
      else
        diff
    }
  })

  def hasNext = {
    @tailrec
    def findNextOverlap : Option[Seq[A]] = {
      if(it.hasNext) {
        val r = it.next
        endValueQueue += r  // enqueue
        sweepLine = tc.start(r)

        // sweep intervals whose end value is less than sweepLine
        while(!endValueQueue.isEmpty && tc.end(endValueQueue.head) < sweepLine) {
          endValueQueue.dequeue
        }
        if(endValueQueue.size > 1)
           Some(endValueQueue.clone.toSeq)
        else
          findNextOverlap
      }
      else
        None
    }

    nextOverlappedSet = nextOverlappedSet.orElse(findNextOverlap)
    nextOverlappedSet.isDefined
  }

  def next() = {
    if(hasNext) {
      val e = nextOverlappedSet.get
      nextOverlappedSet = None
      e
    }
    else
      throw new NoSuchElementException("no more elements")
  }


}