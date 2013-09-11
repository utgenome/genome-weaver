//--------------------------------------
//
// Main.scala
// Since: 2013/09/11 23:17
//
//--------------------------------------

package utgenome.weaver.core

import xerial.lens.cui._
import xerial.core.log.{Logger, LoggerFactory, LogLevel}
import xerial.core.util.StopWatch
import java.io.{BufferedInputStream, FileInputStream, File}
import java.util.Properties
import xerial.core.io.IOUtil
import scala.io.Source

/**
 * @author Taro L. Saito
 */
object Main {
  def main(args:Array[String]) {
    val l = Launcher.of[Main]
    l.execute(args)
  }
}

class Main(@option(prefix="-h,--help", description="display help", isHelp=true)
           help:Boolean,
           @option(prefix="-l,--loglevel", description="Set log level: trace|debug|info|warn|error|fatal")
           logLevel:LogLevel = LogLevel.INFO) extends DefaultCommand with Logger {

  LoggerFactory.setDefaultLogLevel(logLevel)


  private def getVersionFile = {
    val home = System.getProperty("prog.home")
    new File(home, "VERSION")
  }

  private def getVersion : String = {
    val versionFile = getVersionFile
    val versionNumber =
      if (versionFile.exists()) {
        // read properties file
        val prop = (for{
          line <- Source.fromFile(versionFile).getLines
          c = line.split(":=")
          pair <- if(c.length == 2) Some((c(0).trim, c(1).trim)) else None
        } yield pair).toMap

        prop.get("version")
      }
      else
        None

    val v = versionNumber getOrElse "unknown"
    v
  }

  def default = {
    println(s"Genome Weaver:${getVersion}")
    println("Type --help to see the list of sub commands.")
  }

  @command(description="import data")
  def `import`(@argument
               fileName:String) = {

    info(s"import $fileName")
    val s = new StopWatch


    info(s"done. ${s.reportElapsedTime}")
  }


}