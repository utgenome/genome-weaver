package utgenome.glens.collection

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
// PrioritySearchTree.scala
// Since: 2012/07/30 12:01 PM
//
//--------------------------------------

import RedBlackTree._
import collection.mutable
import xerial.core.log.Logging


object GenPrioritySearchTree {

  /**
   * element holder
   * @tparam A
   */
  abstract class Holder[A] extends Iterable[A] {
    def +(h: Holder[A]) : Holder[A] = Multiple(Vector.empty[A] ++ this ++ h)
    def iterator: Iterator[A]
  }

  private[collection] case class Single[A](elem: A) extends Holder[A] {
    override def toString = "[%s]".format(elem)
    def iterator = Iterator.single(elem)
  }

  /**
   * TODO
   * @param elems
   * @tparam A
   */
  private[collection] case class Multiple[A](elems: Vector[A]) extends Holder[A] {
    require(elems.length > 1, "elems must have more than one element")
    override def toString = "[%s]".format(elems.mkString(", "))
    def iterator = elems.iterator
  }


}

object PrioritySearchTree {


  def empty[A](implicit iv: IntervalType[A, Int]) = new PrioritySearchTree[A](null, 0)

  def newBuilder[A](implicit iv: IntervalType[A, Int]): mutable.Builder[A, PrioritySearchTree[A]] = {
    new mutable.Builder[A, PrioritySearchTree[A]] {
      private var tree = PrioritySearchTree.empty[A]
      def +=(elem: A) = {
        tree += elem
        this
      }
      def clear() {  tree = PrioritySearchTree.empty[A]  }
      def result() = tree
    }
  }

  def apply[A](elems: A*)(implicit iv: IntervalType[A, Int]): PrioritySearchTree[A] = {
    val b = newBuilder[A]
    elems foreach { b += _ }
    b.result
  }

}


object LPrioritySearchTree {


  def empty[A](implicit iv: IntervalType[A, Long]) = new LPrioritySearchTree[A](null, 0)

  def newBuilder[A](implicit iv: IntervalType[A, Long]): mutable.Builder[A, LPrioritySearchTree[A]] = {
    new mutable.Builder[A, LPrioritySearchTree[A]] {
      private var tree = LPrioritySearchTree.empty[A]
      def +=(elem: A) = {
        tree += elem
        this
      }
      def clear() {  tree = LPrioritySearchTree.empty[A]  }
      def result() = tree
    }
  }

  def apply[A](elems: A*)(implicit iv: IntervalType[A, LInt]): LPrioritySearchTree[A] = {
    val b = newBuilder[A]
    elems foreach { b += _ }
    b.result
  }

}



import GenPrioritySearchTree._


/**
 * Persistent balanced priority search tree implementation. x-values (interval's start points) are maintained in binary search tree, and the y-values (interval's end points) of the node in the path from the root to leaves
 * are sorted in descending order. This property is good for answering 3-sided queries [x1, x2] x [y1, infinity).
 *
 * This priority search tree allows insertion of the same intervals.
 *
 *
 * @param tree
 * @param size
 * @param iv
 * @tparam A
 */
class PrioritySearchTree[A](tree: Tree[Interval, Holder[A]], override val size: Int)
                           (implicit iv: IntervalType[A, Int])
  extends GenPrioritySearchTree[A, Interval, Int, PrioritySearchTree[A]](tree, size)(iv, Ordering.Int) {

  protected[this] def newTreeBuilder : mutable.Builder[A, PrioritySearchTree[A]] = PrioritySearchTree.newBuilder[A]

  protected def isSmaller(a: Interval, b: Interval): Boolean = a.start < b.start 
  
  protected def createKeyFrom(e:A) : Interval = Interval(iv.start(e), iv.end(e))
  
  protected def newTree(tree:Tree[Interval, Holder[A]], size:Int) = new PrioritySearchTree(tree, size)
}

/**
 * Priority search tree for [[utgenome.glens.collection.LInterval]]
 * @param tree
 * @param size
 * @param iv
 * @tparam A
 */
class LPrioritySearchTree[A](tree: Tree[LInterval, Holder[A]], override val size: Int)
                            (implicit iv: IntervalType[A, Long])
  extends GenPrioritySearchTree[A, LInterval, Long, LPrioritySearchTree[A]](tree, size)(iv, Ordering.Long) {

  protected[this] def newTreeBuilder : mutable.Builder[A, LPrioritySearchTree[A]] = LPrioritySearchTree.newBuilder[A]

  protected def isSmaller(a: LInterval, b: LInterval): Boolean = a.start < b.start
  protected def createKeyFrom(e:A) : LInterval = LInterval(iv.start(e), iv.end(e))
  protected def newTree(tree:Tree[LInterval, Holder[A]], size:Int) = new LPrioritySearchTree(tree, size)
}


case class PSTKey[A, V]()

/**
 * A base class of the persistent priority search tree.
 *
 * @param tree
 * @param size
 * @param iv
 * @tparam A element type
 * @tparam K Augumented Red-black tree node type
 * @tparam V endpoint type of the Interval (Int or Long)
 */
class GenPrioritySearchTree[A, K <: GenInterval[K, V], V, Repr](tree: Tree[K, Holder[A]], override val size: Int)(implicit iv: IntervalType[A, V], ord:Ordering[V])
  extends RedBlackTree[K, Holder[A]] with Iterable[A] with Logging { self =>  

  protected[this] def newTreeBuilder : mutable.Builder[A, Repr]
  
  protected def root: Tree[K, Holder[A]] = if (tree == null) Empty else tree
  protected def isSmaller(a: K, b: K): Boolean
  protected def newTree(tree:Tree[K, Holder[A]], size:Int) : Repr
  protected def createKeyFrom(e:A) : K
  
  protected def updateValue(current:Holder[A], newValue:Holder[A]): Holder[A] = current + newValue

  override def toString = tree.toString

  /**
   * Create a new key so that it becomes the y-upper bound of the children
   * @param a
   * @param b
   * @return
   */
  override protected def mergeKeys(a: K, b:K): K = a.yUpperBound(b)


  /**
   * Return a new tree appending a new element e to the tree.
   * @param e
   * @return
   */
  def +(e: A) : Repr = newTree(root.update(createKeyFrom(e), Single(e)), size + 1)

  /**
   * @return maximum height of the tree
   */
  def height = {
    def height(t: Tree[K, Holder[A]], h: Int): Int = {
      if (t.isEmpty)
        h
      else
        math.max(height(t.left, h + 1), height(t.right, h + 1))
    }

    height(root, 0)
  }


  def iterator = root.iterator.flatMap(_._2)

  def get[A1 <: A](e: A): Option[A] = {
    root.lookup(createKeyFrom(e)) match {
      case Empty => None
      case t => t.value.find(iv.==(_, e))
    }
  }

  def intersectWith(pos:V) : Iterator[A] = intersectWith(Interval(pos, pos))

  
  /**
   * Report the intervals in the tree intersecting with the given range.
   * The result intervals are sorted by their start values in ascending order
   * @param range
   * @return
   */
  def intersectWith[R](range: R)(implicit iv2:IntervalType[R, V]): Repr = {
    val b = newTreeBuilder
    def find(t: Tree[K, Holder[A]]) {
      trace("find range:%s, at key node:%s (left:%s, right:%s)", range, t.key, t.left.key, t.right.key)
      if (t.isEmpty || iv2.compareXY(range, t.key) > 0) {
        // This tree contains no answer since yUpperBound (t.key.x) < range.x
      }
      else {
        def elementInThisNode = t.value.filter(iv.intersect(_, range))
        t.left.map(find) 
        b ++= elementInThisNode
        if (ord.compare(t.key.start, iv2.end(range)) <= 0) 
          t.right.map(find) 
      }
    }

    find(root)
    b.result
  }

  override def first = {
    def findFirst(t: Tree[K, Holder[A]]): A = {
      if (t.isEmpty)
        null.asInstanceOf[A]
      else {
        val l = findFirst(t.left)
        if (l != null)
          l
        else
          t.value.head
      }
    }

    findFirst(root)
  }

  override def last = {
    def findLast(t: Tree[K, Holder[A]]): A = {
      if (t.isEmpty)
        null.asInstanceOf[A]
      else {
        val r = findLast(t.right)
        if (r != null)
          r
        else
          t.value.last
      }
    }

    findLast(root)
  }

  def range(from: Option[V], until: Option[V]): Repr = {
    val b = newTreeBuilder

    def takeValue(t: Tree[K, Holder[A]]): Iterator[A] = {
      if (t.isEmpty)
        Iterator.empty
      else
        t.left.map(takeValue) ++ t.value.iterator ++ t.right.map(takeValue)
    }

    def find(t: Tree[K, Holder[A]]) {
      if (t.isEmpty)
        Iterator.empty
      else {
        (from, until) match {
          case (None, None) => b ++= t.map(takeValue)
          case (Some(s), _) if ord.compare(t.key.start, s) < 0 => find(t.right)
          case (_, Some(e)) if ord.compare(e, t.key.start) < 0 => find(t.left)
          case _ => {
            find(t.left)
            b ++= t.value
            find(t.right)
          }
        }
      }
    }

    find(root)
    b.result
  }
  
  def from(v:V) : Repr = range(Some(v), None)
  def until(v:V) : Repr = range(None, Some(v))
}


