//--------------------------------------
//
// GLensSpec.scala
// Since: 2013/03/01 10:55
//
//--------------------------------------

package utgenome.glens

import org.scalatest.matchers.{MustMatchers, ShouldMatchers}
import xerial.core.util.Timer
import xerial.core.log.Logger
import org.scalatest.{Tag, OptionValues, GivenWhenThen, WordSpec}
import java.io.ByteArrayOutputStream
import scala.language.implicitConversions
import xerial.core.io.Resource


/**
 * @author leo
 */
trait GLensSpec extends WordSpec with ShouldMatchers with MustMatchers with GivenWhenThen with OptionValues with Resource with Timer with Logger {

  implicit def toTag(t:String) = Tag(t)

  /**
   * Captures the output stream and returns the printed messages as a String
   * @param body
   * @tparam U
   * @return
   */
  def captureOut[U](body: => U) : String = {
    val out = new ByteArrayOutputStream
    Console.withOut(out) {
      body
    }
    new String(out.toByteArray)
  }

  def captureErr[U](body: => U) : String = {
    val out = new ByteArrayOutputStream
    Console.withErr(out) {
      body
    }
    new String(out.toByteArray)
  }


}
