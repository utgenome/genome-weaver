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

package utgenome.weaver.core

import java.io.File
import io.Source
import xerial.lens.Eq
import xerial.core.log.Logger


//--------------------------------------
//
// BED.scala
// Since: 2012/03/16 14:02
//
//--------------------------------------

object BED {

  def parse(line: String): BED = {
    val c = line.split("\\s+")
    // set to 1-origin
    new BED(c(0), c(1).toInt + 1, c(2).toInt + 1, Strand(c(3)))
  }

}



/**
 * UCSC's BED format
 * @param chr
 * @param start
 * @param end
 */
class BED(override val chr: String, override val start: Int, override val end: Int, override val strand: Strand)
  extends GInterval(chr, start, end, strand) {

}

/**
 * BED full entry. Original BED file is zero-origin, but this class uses one-origin for the compatibility with the other biological data formats
 *
 * @author leo
 */
class BEDGene
  (
  override val chr: String,
  override val start: Int,
  override val end: Int,
  override val strand: Strand,
  val name: String,
  val score: Int,
  val thickStart: Int,
  val thickEnd: Int,
  val itemRgb: String,
  val blockCount: Int,
  val blockSizes: Array[Int],
  val blockStarts: Array[Int]
)
  extends GInterval(chr, start, end, strand) with Eq {

  override def toString = "%s %s[%s,%s]".format(name, chr, start, end)

  private def concatenate(blocks: Array[Int]): String = {
    val b = new StringBuilder
    blocks.foreach {
      e => b.append(e.toString); b.append(",")
    }
    b.toString
  }

  /**
   * Output this data as a 0-origin BED data line
   * @return
   */
  def toBEDLine: String = (chr, start - 1, end - 1, name, score, strand, thickStart - 1, thickEnd - 1, if (itemRgb != null) itemRgb else "0", blockCount, concatenate(blockSizes), concatenate(blockStarts)).productIterator.mkString("\t")

  def isForward = strand.isForward
  def isReverse = strand.isReverse

  def cdsFivePrimeEnd = cdsRange.fivePrimeEnd
  def cdsThreePrimeEnd = cdsRange.threePrimeEnd

  def cdsStart = cdsRange.start
  def cdsEnd = cdsRange.end
  lazy val cdsRange: GInterval = new GInterval(chr, thickStart, thickEnd, strand)
  lazy val exons: Array[GInterval] = {
    for ((size, exonStart) <- blockSizes.zip(blockStarts)) yield {
      val s = start+exonStart
      new GInterval(chr, s, s+size, strand)
    }
  }
  lazy val cds: Array[GInterval] = {
    for (ex <- exons; c <- ex.intersection(cdsRange)) yield {
      c
    }
  }

  private def extractIntrons(fragments:Array[GInterval]) : Array[GInterval] = {
    val intronSeq = for(i <- 0 until fragments.length-1) yield {
      val prev = fragments(i)
      val next = fragments(i+1)
      new GInterval(prev.chr, prev.end, next.start, prev.strand)
    }
    intronSeq.toArray
  }

  lazy val introns : Array[GInterval] = extractIntrons(exons)
  lazy val cdsIntrons : Array[GInterval] = extractIntrons(cds)

  def firstExon: Option[GInterval] = {
    strand match {
      case Forward => exons.headOption
      case Reverse => exons.lastOption
    }
  }

  def lastExon: Option[GInterval] = {
    strand match {
      case Forward => exons.lastOption
      case Reverse => exons.headOption
    }
  }

}



object BEDGene extends Logger {

  def apply(line: String): BEDGene = {
    def parseBlock(blocks: String) = {
      val c = blocks.trim.stripSuffix(",").split(",")
      if(c.isEmpty)
        Array.empty[Int]
      else {
        val r = for(b <- c if b.length() > 0) yield b.toInt
        r.toArray[Int]
      }
    }

    val col = line.split("\\s+")
    val c = (0 to 11).map {
      i => if (i < col.length) Some(col(i)) else None
    }
    // (chr, start-1, end-1, name, score, strand, thickStart-1, thickEnd-1, if(itemRgb != null) itemRgb else "0", blockCount, concatenate(blockSizes), concatenate(blockStarts))
    val chr = c(0).getOrElse("")
    val start = c(1).getOrElse("0").toInt + 1 // convert to 1-origin
    val end = c(2).getOrElse("0").toInt + 1  // convert to 1-origin
    val name = c(3).getOrElse("")
    val score = c(4).getOrElse("0").toInt
    val strand = Strand(c(5).getOrElse("+"))
    val thickStart = c(6).getOrElse(start.toString).toInt + 1
    val thickEnd = c(7).getOrElse(end.toString).toInt + 1
    val itemRgb = c(8).getOrElse("0")
    val blockCount = c(9).getOrElse("0").toInt
    val blockSizes = parseBlock(c(10).getOrElse(""))
    val blockOffsets = parseBlock(c(11).getOrElse(""))
    new BEDGene(chr, start, end, strand, name, score, thickStart, thickEnd, itemRgb, blockCount, blockSizes, blockOffsets)
  }

  def parse(file:File) = {
    for((line, lineNum) <- Source.fromFile(file).getLines().zipWithIndex if !line.startsWith("track")) yield {
      try {
        BEDGene(line)
      }
      catch {
        case e : Exception => {
          error("error occurred at line %d: %s", lineNum+1, line)
          throw e
        }
      }
    }
  }
}

