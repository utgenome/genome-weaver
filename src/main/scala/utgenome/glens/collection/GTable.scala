package utgenome.glens.collection

import collection.mutable
import utgenome.glens.GInterval


//--------------------------------------
//
// GTable.scala
// Since: 2012/08/24 5:34 PM
//
//--------------------------------------




/**
 * list of GIntervals
 *
 * @author leo
 */
class GTable[A <: GInterval] {
  
  private val table = mutable.Map[String, PrioritySearchTree[A]]()



}