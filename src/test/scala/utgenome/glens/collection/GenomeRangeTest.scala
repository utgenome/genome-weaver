/*
 * Copyright 2012 Taro L. Saito
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package utgenome.glens.collection


import util.Random
import xerial.core.XerialSpec
import utgenome.glens.{Strand, Reverse, Forward}
import utgenome.glens.collection.GInterval.GIntervalTypeBase

//--------------------------------------
//
// GenomeRangeTest.scala
// Since: 2012/04/02 16:05
//
//--------------------------------------

/**
 * @author leo
 */
class GenomeRangeTest extends XerialSpec {

  import xerial.core.util.StopWatch._

  "interval" should {
    "have equality for the same range" in {
      val i1 : Interval = new Interval(1, 100)
      val i2 : Interval = new Interval(1, 100)
      i1.hashCode must be(i2.hashCode)

      i1 should be (i2)
      val i3 = new Interval(1, 109)
      i3 should not be (i1)
    }

    "have no significant performance overhead" in {
      val r = new Random
      val N = 1000

      time("new") {
        for (i <- 0 until N) {
          val s = r.nextInt(100000).abs
          val e = s + r.nextInt(200000).abs
          new Interval(s, e)
        }
      }
    }

  }

  "GInterval" should {
    "satisfy equality" in {
      val g1 = new GInterval("chr1", 34, 140, Forward)
      val g2 = new GInterval("chr1", 34, 140, Forward)
      g1.hashCode must be(g2.hashCode)
      g1 should be(g2)

      // compare different type of objects
      val g3 = new Interval(34, 140)
      g3 should not be (g1)
      g3.hashCode must not be (g1.hashCode)
    }

    "allow type extention" in {

      class MyGInterval(chr:String, start:Int, end:Int, strand:Strand) extends GInterval(chr, start, end, strand)
      implicit object MyGIntevalType extends GIntervalTypeBase[MyGInterval] {
        def newInterval(base: MyGInterval, newStart: Int, newEnd: Int) = new MyGInterval(base.chr, newStart, newEnd, base.strand)
      }

      var p = PrioritySearchTree.empty[MyGInterval]
      p += new MyGInterval("chr1", 1, 200, Forward)

    }


  }

  "GLocus" should {
    "satisfy equality" in {
      val l1 = new GLocus("chr1", 134134, Forward)
      val l2 = new GLocus("chr1", 134134, Forward)
      l1 must be(l2)
      l1.hashCode must be(l2.hashCode)

      val l3 = new GLocus("chr2", 134134, Forward)
      val l4 = new GLocus("chr1", 134134, Reverse)
      l1 must not be (l3)
      l1.hashCode must not be (l3.hashCode)
      l1 must not be (l4)
      l1.hashCode must not be (l4.hashCode)
    }
  }

}