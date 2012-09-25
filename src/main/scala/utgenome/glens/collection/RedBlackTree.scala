package utgenome.glens.collection

//--------------------------------------
//
// RedBlackTree.scala
// Since: 2012/08/21 2:27 PM
//
//--------------------------------------

import xerial.core.log.Logging

object RedBlackTree {

  /**
   * Represents nodes in RedBlackTrees
   * @tparam A
   * @tparam B
   */
  abstract class Tree[A, B] {
    def isEmpty: Boolean
    def isBlack: Boolean
    def left: Tree[A, B]
    def right: Tree[A, B]
    def iterator: Iterator[(A, B)] = Iterator.empty
    def key: A
    def value: B
    def getKey: Option[A] = None
    def update(k: A, v: B): Tree[A, B]
    def insert(k: A, v: B): Tree[A, B]
    def lookup(e: A): Tree[A, B]
    def map[C](f: Tree[A, B] => C): C = f(this)
    def foreach[C](f: Tree[A, B] => C): Unit
  }

}

import RedBlackTree._

/**
 * Base class for implementing data structures based on Red-Black trees.
 *
 * Balancing operations (balanceLeft, balanceRight) are based on Okasaki's idea (See also Purely functional data structures by C. Okasaki)
 *
 * TODO:
 * deletion (Kahrs 2001)
 * union (for range operation) (building RedBlackTrees from sorted list in linear time. Appel 2011)
 *
 * @tparam A key type
 * @tparam B value type associated to the key
 */
abstract class RedBlackTree[A, B] extends Logging {

  /**
   * Compare keys
   * @param a
   * @param b
   * @return
   */
  protected def isSmaller(a: A, b: A): Boolean

  //protected def updateTree(t: Tree[A, B], key: A, value: B): Tree[A, B]

  /**
   * Create a new key by merging two keys
   * @param key current key
   * @param other the other key to merge
   * @return new key
   */
  protected def mergeKeys(key: A, other: A): A = key
  protected def mergeKeys(key:A, other:Option[A]) : A = other.map { mergeKeys(key, _) } getOrElse key
  protected def mergeKeys(key: A, lkey: Option[A], rkey: Option[A]): A =  mergeKeys(mergeKeys(key, lkey), rkey)

  /**
   * Update the node value. This method is used for updating the contents of tree nodes without changing the tree structure.
   * @param current
   * @param newValue
   * @return
   */
  protected def updateValue(current:B, newValue:B) : B

  def mkTree(isBlack: Boolean, key: A, h: B, l: Tree[A, B], r: Tree[A, B]): Tree[A, B] = {
    if (isBlack)
      BlackTree(key, h, l, r)
    else
      RedTree(key, h, l, r)
  }

  def blacken(t: Tree[A, B]): Tree[A, B] = t match {
    case RedTree(k, e, l, r) => BlackTree(k, e, l, r)
    case _ => t
  }

  object Empty extends Tree[A, B] {
    override def toString = "Empty"
    def value = throw new NoSuchElementException("Empty node has no value")
    def isEmpty = true
    def isBlack = true
    def left = Empty
    def right = Empty
    def key = null.asInstanceOf[A]
    def update(k: A, v: B) = blacken(insert(k, v))
    def insert(k: A, v: B) = RedTree(k, v, Empty, Empty)
    def lookup(e: A): Tree[A, B] = this
    def foreach[C](f: Tree[A, B] => C): Unit = {}
  }

  abstract class NonEmpty extends Tree[A, B] {
    def isEmpty = false
    override def iterator: Iterator[(A, B)] = left.iterator ++ Iterator.single((key, value)) ++ right.iterator
    override def getKey = Some(key)
    def value: B
    def update(k: A, v: B): Tree[A, B] = blacken(insert(k, v))
    def insert(k: A, v: B): Tree[A, B] = {
      if (isSmaller(k, key))
        balanceLeft(isBlack, key, value, left.insert(k, v), right)
      else if (isSmaller(key, k))
        balanceRight(isBlack, key, value, left, right.insert(k, v))
      else
        mkTree(isBlack, mergeKeys(key, k), updateValue(this.value, v), left, right) // k.x == this.key.x
    }
    private def balanceLeft(isBlack: Boolean, z: A, zv: B, l: Tree[A, B], r: Tree[A, B]): Tree[A, B] = l match {
      case RedTree(y, yv, RedTree(x, xv, a, b), c) =>
        RedTree(mergeKeys(y, Some(x), Some(z)), yv, BlackTree(x, xv, a, b), BlackTree(z, zv, c, r))
      case RedTree(x, xv, a, RedTree(y, yv, b, c)) =>
        RedTree(mergeKeys(y, Some(x), Some(z)), yv, BlackTree(x, xv, a, b), BlackTree(z, zv, c, r))
      case _ =>
        mkTree(isBlack, mergeKeys(z, l.getKey, r.getKey), zv, l, r)
    }
    private def balanceRight(isBlack: Boolean, x: A, xv: B, l: Tree[A, B], r: Tree[A, B]): Tree[A, B] = r match {
      case RedTree(z, zv, RedTree(y, yv, b, c), d) =>
        RedTree(mergeKeys(y, Some(x), Some(z)), yv, BlackTree(x, xv, l, b), BlackTree(z, zv, c, d))
      case RedTree(y, yv, b, RedTree(z, zv, c, d)) =>
        RedTree(mergeKeys(y, Some(x), Some(z)), yv, BlackTree(x, xv, l, b), BlackTree(z, zv, c, d))
      case _ =>
        mkTree(isBlack, mergeKeys(x, l.getKey, r.getKey), xv, l, r)
    }
    def lookup(e: A): Tree[A, B] = {
      if (isSmaller(e, key))
        left.lookup(e)
      else if (isSmaller(key, e))
        right.lookup(e)
      else
        this
    }
    def foreach[C](f: Tree[A, B] => C): Unit = {
      left.foreach(f)
      f(this)
      right.foreach(f)
    }
  }

  case class RedTree(override val key: A, value: B, left: Tree[A, B], right: Tree[A, B]) extends NonEmpty {
    def isBlack = false
  }

  case class BlackTree(override val key: A, value: B, left: Tree[A, B], right: Tree[A, B]) extends NonEmpty {
    def isBlack = true
  }


}