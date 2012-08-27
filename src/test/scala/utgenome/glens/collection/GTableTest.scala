//--------------------------------------
//
// GTableTest.scala
// Since: 2012/08/27 6:07 PM
//
//--------------------------------------

package utgenome.glens.collection

import xerial.core.XerialSpec

/**
 * @author leo
 */
class GTableTest extends XerialSpec {
  "GTable" should {

    trait Sample {
      val g = new GTable[GInterval]
      g += GInterval("chr1", 100, 344)
      g += GInterval("chr2", 213, 500)
      g += GInterval("chr1", 140, 499)
    }

    "hold GInterval" in {
      new Sample {
        debug(g.mkString(", "))
        g should have size 3
        g("chr1") should have size 2
        g("chr2") should have size 1

        val isect = g.intersectWith(GInterval("chr1", 130, 135)).toSeq
        isect should have size 1
        isect.head should be(GInterval("chr1", 100, 344))


        g.chrSet should be(Set("chr1", "chr2"))
      }
    }

    "allow par operation" in {
      new Sample {
        val v = g.par.map(_.toString)
        debug(v.mkString(", "))

        v should have size 3
      }
    }
  }
}