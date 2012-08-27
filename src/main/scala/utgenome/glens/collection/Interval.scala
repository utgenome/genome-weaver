package utgenome.glens.collection

import xerial.core.lens.Eq


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

//--------------------------------------
//
// Interval.scala
// Since: 2012/07/30 11:21 AM
//
//--------------------------------------

/**
 * Type class for two-dimensional points
 *
 * @tparam A
 * @tparam V
 */
trait Point2D[A, @specialized(Int, Long) V] extends Ordering[A] {
  def x(a: A): V
  def y(a: A): V

  /**
   * Ordering function of type V values
   * @return
   */
  def ord : OrderingOpt[V]

  def compare(a: A, b: A): Int = {
    val diff = compareX(a, b)
    if (diff == 0) compareY(a, b) else diff
  }

  def ==(a: A, b: A): Boolean = xEquals(a, b) && yEquals(a, b)

  def compareX(a: A, b: A): Int = ord.compare(x(a), x(b))
  def compareY(a: A, b: A): Int = ord.compare(y(a), y(b))
  def compareXY(a: A, b: A): Int = ord.compare(x(a), y(b))
  def xIsSmaller(a: A, b: A): Boolean = compareX(a, b) < 0
  def xEquals(a: A, b: A): Boolean = compareX(a, b) == 0
  def yIsSmaller(a: A, b: A): Boolean = compareY(a, b) < 0
  def yEquals(a: A, b: A): Boolean = compareY(a, b) == 0
  def xIsSmallerThanOrEq(a: A, b: A): Boolean = compareX(a, b) <= 0
  def yIsSmallerThanOrEq(a: A, b: A): Boolean = compareY(a, b) <= 0

}

/**
 * Type class representing intervals
 * @tparam A  actual class
 * @tparam V  interval value type (Usually it is Int or Long)
 * @author leo
 */
trait IntervalType[A, @specialized(Int, Long) V] extends Point2D[A, V] {

  def x(a: A) = start(a)
  def y(a: A) = end(a)
  def start(a: A): V
  def end(a: A): V

  def precede(a: A, b: A): Boolean = ord.lteq(end(a), start(b))
  def follow(a: A, b: A): Boolean = ord.lteq(end(b), start(a))
  def intersect(a: A, b: A): Boolean = ord.lteq(start(a), end(b)) && ord.lteq(start(b), end(a))

  /**
   * Take the intersection of two intervals
   */
  def intersection(a:A, b:A) : Option[A] = {
    val s = ord.max(start(a), start(b))
    val e = ord.min(end(a), end(b))
    if(ord.lteq(s, e))
      Some(newInterval(a, s, e))
    else
      None
  }


  def contain(a: A, b: A): Boolean = ord.lteq(start(a), start(b)) && ord.lteq(end(b), end(a))
  def containPoint(a: A, p: V) : Boolean = ord.lteq(start(a), p) && ord.lteq(p, end(a))

  def startIsSmaller(a: A, b: A): Boolean = ord.lt(start(a), start(b))
  def endIsSmaller(a: A, b: A): Boolean = ord.lt(end(a), end(b))

  /**
   * Used in PrioritySearchTrees to create parent nodes
   * @param a
   * @param b
   * @return
   */
  def yUpperBound(a: A, b: A): A = {
    newInterval(a, start(a), if(yIsSmaller(a, b)) end(b) else end(a))
  }
  def newInterval(base:A, newStart:V, newEnd:V) : A
}

abstract class IntIntervalType[A] extends IntervalType[A, Int] {
  /**
   * Ordering function of type V values
   * @return
   */
  def ord = OrderingOpt.IntOrd
}

abstract class LongIntervalType[A] extends IntervalType[A, Long] {
  /**
   * Ordering function of type V values
   * @return
   */
  def ord = OrderingOpt.LongOrd
}


/**
 * Generic interval is a common trait of Interval and LInterval classes
 * @tparam Repr representation
 * @tparam V value type of the interval coordinates
 */
trait GenInterval[Repr, V] extends Eq { this : Repr =>
  protected def intervalType : IntervalType[Repr, V]
  override def toString = "%d:%d".format(intervalType.start(this), intervalType.end(this))

  def size : V = intervalType.ord.diff(intervalType.end(this), intervalType.start(this))

  /**
   * Detect overlaps with the specified interval, including containment
   * @param other
   * @return
   */
  def intersectWith[A <: Repr](other: A): Boolean = intervalType.intersect(this, other)
  def contains[A <: Repr](other: A): Boolean = intervalType.contain(this, other)
  def containsPoint(pos: V): Boolean = intervalType.containPoint(this, pos)
  def intersection[A <: Repr](other:A): Option[Repr] = intervalType.intersection(this, other)
}


/**
 * Closed interval [start, end], where start and end are Int values
 */
class Interval(val start: Int, val end: Int) extends GenInterval[Interval, Int]  {
  require(start <= end, "start must be smaller than or equals to end: [%d, %d]".format(start, end))
  protected def intervalType = Interval.IntervalType

  def toRange: Range = Range(start, end)
}

/**
 * Closed interval [start, end] where start and end are Long values
 * @param start
 * @param end
 */
class LInterval(val start: Long, val end: Long) extends GenInterval[LInterval, Long] {
  require(start <= end, "start must be smaller than or equals to end: [%d, %d]".format(start, end))

  protected def intervalType = LInterval.IntervalType

}

object Interval {
  implicit object IntervalType extends IntervalType[Interval, Int] {
    def start(a: Interval) = a.start
    def end(a: Interval) = a.end

    def newInterval(base:Interval, newStart:Int, newEnd:Int) : Interval = {
      new Interval(newStart, newEnd)
    }

    def ord = OrderingOpt.IntOrd
  }

  object IntervalOrdering extends Ordering[Interval] {
    def compare(x: Interval, y: Interval) = {
      val diff = x.start - y.start
      if(diff == 0)
        x.end - y.end
      else
        diff
    }
  }


  def apply(s: Int, e: Int) = new Interval(s, e)
  def point(s: Int) = new Interval(s, s)
}


object LInterval {

  implicit object IntervalType extends IntervalType[LInterval, Long] {
    def ord = OrderingOpt.LongOrd
    def start(a: LInterval) = a.start
    def end(a: LInterval) = a.end
    def newInterval(base:LInterval, newStart:Long, newEnd:Long) : LInterval = {
      new LInterval(newStart, newEnd)
    }
  }

  def apply(s: Long, e: Long) = new LInterval(s, e)
  def point(s: Long) = new LInterval(s, s)
}

