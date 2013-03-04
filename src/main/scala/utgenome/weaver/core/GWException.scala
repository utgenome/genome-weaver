//--------------------------------------
//
// GWException.scala
// Since: 2013/03/04 12:52 PM
//
//--------------------------------------

package utgenome.weaver.core

/**
 * @author Taro L. Saito
 */
object GWException {



  abstract class ParseError(msg:String) extends Exception(msg)
  case class SyntaxError(msg:String) extends ParseError(msg)

}