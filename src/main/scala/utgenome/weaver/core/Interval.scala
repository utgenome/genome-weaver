package utgenome.weaver.core

import xerial.lens.Eq


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
 */
trait Point2D[A] extends Ordering[A] {
  def x(a: A): Int
  def y(a: A): Int


  def compare(a: A, b: A): Int = {
    val diff = x(a) - x(b)
    if (diff == 0) y(a) - y(b) else diff
  }

  def ==[B](a: A, b: B)(implicit iv:Point2D[B]): Boolean = xEquals(a, b) && yEquals(a, b)

  def maxX[B](a: A, b: B)(implicit iv:Point2D[B]) : Int = if(compareX(a, b)<0) iv.x(b) else x(a)
  def maxY[B](a: A, b: B)(implicit iv:Point2D[B]) : Int = if(compareY(a, b)<0) iv.y(b) else y(a)
  
  def compareX[B](a: A, b: B)(implicit iv:Point2D[B]): Int = x(a) - iv.x(b)
  def compareY[B](a: A, b: B)(implicit iv:Point2D[B]): Int = y(a) - iv.y(b)
  def compareXY[B](a: A, b: B)(implicit iv:Point2D[B]): Int = x(a) - iv.y(b)
  def xIsSmaller[B](a: A, b: B)(implicit iv:Point2D[B]): Boolean = compareX(a, b) < 0
  def xEquals[B](a: A, b: B)(implicit iv:Point2D[B]): Boolean = compareX(a, b) == 0
  def yIsSmaller[B](a: A, b: B)(implicit iv:Point2D[B]): Boolean = compareY(a, b) < 0
  def yEquals[B](a: A, b: B)(implicit iv:Point2D[B]): Boolean = compareY(a, b) == 0
  def xIsSmallerThanOrEq[B](a: A, b: B)(implicit iv:Point2D[B]): Boolean = compareX(a, b) <= 0
  def yIsSmallerThanOrEq[B](a: A, b: B)(implicit iv:Point2D[B]): Boolean = compareY(a, b) <= 0

}


/**
 * Type class representing intervals
 * @tparam A  actual class
 * @author leo
 */
trait IntervalType[A] extends Point2D[A] {

  def x(a: A) = start(a)
  def y(a: A) = end(a)
  def start(a: A): Int
  def end(a: A): Int

  def precede[B](a: A, b: B)(implicit iv:IntervalType[B]): Boolean = end(a) <= iv.start(b)
  def follow[B](a: A, b: B)(implicit iv:IntervalType[B]): Boolean = iv.end(b) <= start(a)
  def intersect[B](a: A, b: B)(implicit iv:IntervalType[B]): Boolean = (start(a) <= iv.end(b)) && (iv.start(b) <= end(a))

  /**
   * Take the intersection of two intervals
   */
  def intersection[B](a:A, b:B)(implicit iv:IntervalType[B]) : Option[Interval] = {
    val s = math.max(start(a), iv.start(b))
    val e = math.min(end(a), iv.end(b))
    if(s <= e)
      Some(Interval(s, e))
    else
      None
  }


  def contain[B](a: A, b: B)(implicit iv:IntervalType[B]): Boolean = (start(a) <= iv.start(b)) && (iv.end(b) <= end(a))
  def containPoint(a: A, p: Int) : Boolean = (start(a) <= p) && (p <= end(a))

  def startIsSmaller[B](a: A, b: B)(implicit iv:IntervalType[B]): Boolean = start(a) < iv.start(b)
  def endIsSmaller[B](a: A, b: B)(implicit iv:IntervalType[B]): Boolean = end(a) < iv.end(b)

}



/**
 * Generic interval is a common trait of Interval and LInterval classes
 * @tparam Repr representation
 */
trait GenInterval[Repr <: GenInterval[Repr]] extends Eq { this : Repr =>
  protected def intervalType : IntervalType[Repr]
  override def toString = "%d:%d".format(start, end)

  def start : Int
  def end : Int
  def size : Int = end - start

  /**
   * Detect overlaps with the specified interval, including containment
   * @param other
   * @return
   */
  def intersectWith[A](other: A)(implicit iv:IntervalType[A]): Boolean = intervalType.intersect(this, other)
  def contains[A](other: A)(implicit iv:IntervalType[A]): Boolean = intervalType.contain(this, other)
  def containsPoint(pos: Int): Boolean = intervalType.containPoint(this, pos)
  def intersection[A](other:A)(implicit iv:IntervalType[A]): Option[Interval] = intervalType.intersection(this, other)

  def yUpperBound(other:Repr) : Repr
}


/**
 * Closed interval [start, end], where start and end are Int values
 */
class Interval(val start: Int, val end: Int) extends GenInterval[Interval]  {
  require(start <= end, "start must be smaller than or equals to end: [%d, %d]".format(start, end))
  protected def intervalType = Interval.IntervalType

  def yUpperBound(other:Interval) = Interval(this.start, math.max(end, other.end))
  def toRange: Range = Range(start, end)
}

object Interval {
  implicit object IntervalType extends IntervalType[Interval] {
    def start(a: Interval) = a.start
    def end(a: Interval) = a.end
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

