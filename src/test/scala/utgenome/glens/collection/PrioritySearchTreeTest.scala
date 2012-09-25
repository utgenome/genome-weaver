package utgenome.glens.collection

//--------------------------------------
//
// PrioritySearchTreeTest.scala
// Since: 2012/07/30 12:53 PM
//
//--------------------------------------

import xerial.core.XerialSpec
import util.Random

/**
 * @author leo
 */
class PrioritySearchTreeTest extends XerialSpec {


  def overlapQuery(p: PrioritySearchTree[Interval], q: Interval) {

    var overlapped: Array[Interval] = null
    var overlapped_ans: Array[Interval] = null

    debug("overlap query: %s", q)
    time("overlap query", repeat = 3) {
      block("pst query") {
        overlapped = p.intersectWith(q).toArray
      }
      block("O(n) search") {
        overlapped_ans = p.filter(_.intersectWith(q)).toArray
      }
    }

    trace("overlapped = %s", overlapped.mkString(", "))
    trace("answer     = %s", overlapped_ans.mkString(", "))


    overlapped should be(overlapped_ans)
  }

  def treeStat(p: PrioritySearchTree[_]) {
    debug("height:%d n:%d, log2(n):%.2f", p.height, p.size, math.log10(p.size) / math.log10(2))
  }

  "PrioritySearchTree" should {
    "insert new nodes" in {
      var p = PrioritySearchTree.empty[Interval]
      p += Interval(3, 5)
      p += Interval(4, 9)
      p += Interval(4, 12)
      p += Interval(4, 9)
      p += Interval(10, 15)
      val p1 = p
      p += Interval(6, 11)
      p += Interval(4, 20)
      p += Interval(20, 25)
      p += Interval(28, 32)
      p += Interval(1, 20)
      debug(p)
      treeStat(p)
      debug(p.mkString(", "))
      debug("p1:%s", p1)

      p.get(Interval(4, 20)) should be('defined)
      p.get(Interval(4, 8)) should be('empty)

      val rng = p.range(Some(5), Some(23))
      debug("range:%s", rng.mkString(", "))

      overlapQuery(p, Interval(6, 10))
      overlapQuery(p, Interval(13, 18))

    }

    "mix GInterval sub classes" in {
      var p = PrioritySearchTree.empty[GInterval]
      p += GInterval("chr1", 20, 40)
      p += GInterval("chr1", 34, 50)
      p += GInterval("chr1", 53, 59)

      debug("tree:%s", p)
      debug("query GIntervals with Interval")
      val r = p.intersectWith(Interval(25, 38)).toList
      debug(r.mkString(", "))
      r.toList should have size (2)
    }



    "insert many nodes" in {
      val r = new Random(0)
      val b = PrioritySearchTree.newBuilder[Interval]
      val n = 10000
      for (i <- 0 until n) {
        val s = r.nextInt(1000000)
        b += Interval(s, s + (100 + r.nextInt(1000)))
      }
      val p = b.result
      p.size should be(n)
      treeStat(p)
      overlapQuery(p, Interval(1000, 1100))

    }


  }

}