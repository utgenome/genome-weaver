//--------------------------------------
//
// FASTAIndex.scala
// Since: 2012/06/22 5:09 PM
//
//--------------------------------------

package utgenome.weaver.core

import org.xerial.snappy.{SnappyInputStream, Snappy, SnappyOutputStream}
import java.io._
import xerial.core.log.Logger
import scala.util.{Failure, Success, Try}
import xerial.core.io.IOUtil
import xerial.larray.{MMapMode, LArray}
import scala.io.Source

/**
 * Indexes of FASTA data
 *
 * @author leo
 */
trait FASTAIndex {

  def apply(seqName: String): DNASeq

  /**
   * Extract the sub sequence of the specified range [start, end).
   * @param seqName sequence name. e.g., chromosome name
   * @param start 0-based index (inclusive)
   * @param end 0-based index (exclusive)
   * @return
   */
  def subSequence(seqName: String, start: Int, end: Int): DNASeq

  def sequenceLength(seqName: String): Int

  def sequenceNames: Iterable[String]
}

object FASTAEntryIndex extends Logger {
  def parse(line:String) : Try[FASTAEntryIndex] = {
    val c = line.split("""\t""")
    Try(FASTAEntryIndex(c(0), c(3), c(1).toLong, c(2).toInt))
  }
}

case class FASTAEntryIndex(name: String, description: String, offset: Long, length: Int) {
  def toTSV = s"${name}\t${offset}\t${length}\t$description"
}

trait FASTAIndexLike[Repr <: DNASeq with DNASeqOps[Repr]] {

  protected val seq : Repr
  protected val entry : Seq[FASTAEntryIndex]

  protected lazy val index: Map[String, FASTAEntryIndex] = (entry.map(e => e.name -> e)).toMap[String, FASTAEntryIndex]

  def sequenceLength(seqName: String) = index.apply(seqName).length

  def sequenceNames = index.keys

  /**
   * Retrieve a DNASeq of the given name
   * @param seqName
   * @return
   */
  def apply(seqName: String): DNASeq = new WrappedFASTASeq[Repr](seq, index(seqName).offset, sequenceLength(seqName))

  /**
   * Extract the sub sequence of the specified range [start, end).
   * @param seqName sequence name. e.g., chromosome name
   * @param start 0-based index (inclusive)
   * @param end 0-based index (exclusive)
   * @return
   */
  def subSequence(seqName: String, start: Int, end: Int): DNASeq = {
    val globalIndex = index(seqName).offset + start
    new WrappedFASTASeq[Repr](seq, globalIndex, end - start)
  }
}

class FASTAIndex2bit(protected val seq: ACGTSeq, protected val entry: Seq[FASTAEntryIndex])
  extends FASTAIndex
  with FASTAIndexLike[ACGTSeq] {

}

object FASTAIndex3bit extends Logger {
  def loadFrom(file:String) : FASTAIndex3bit = {

    val f = new File(file)
    var cursor = 0L
    val mmap = LArray.mmap(f, 0, f.length(), MMapMode.READ_ONLY)

    // Read FASTAEntryIndex
    val compressedDataSize = mmap.getInt(0)
    cursor += 4

    val uncompressedSize = Snappy.uncompressedLength(mmap.address + cursor, compressedDataSize)
    val buf = LArray.of[Byte](uncompressedSize)
    Snappy.rawUncompress(mmap.address + cursor, compressedDataSize, buf.address)
    cursor += compressedDataSize

    val indexes = Source.fromString(new String(buf.toArray)).getLines
      .map(FASTAEntryIndex.parse)
      .collect {
        case Success(entry) => entry
        case Failure(err) => warn(err)
      }


    null
  }

}


class FASTAIndex3bit(protected val seq: ACGTNSeq, protected val entry: Seq[FASTAEntryIndex])
  extends FASTAIndex
  with FASTAIndexLike[ACGTNSeq] {

  def saveTo(file:String) {
    val f = new File(file)
    IOUtil.withResource(LArray.mmap(f, 0, 0, MMapMode.READ_WRITE)) { out =>
      val b = new StringBuilder
      for(e <- entry) yield {
        b.append(e.toTSV)
        b.append("\n")
      }
      val compressedIndex = Snappy.compress(b.result)
      var cursor = 0
      out.putInt(out.address + cursor, compressedIndex.size)
      cursor += 4
      out.readFromArray(compressedIndex, 0, cursor, compressedIndex.size)
      cursor += compressedIndex.size
      seq.saveTo(out, cursor)
    }
  }

}


/**
 * Wrapped DNASeq
 *
 * @param seq
 * @param offset
 * @param length
 */
class WrappedFASTASeq[Repr <: DNASeq with DNASeqOps[Repr]](seq: Repr, offset: Long, length: Long)
  extends DNASeq
  with DNASeqOps[Repr] {

  def domain = seq.domain

  def apply(index: Long) = seq(offset + index)

  def slice(start: Long, end: Long) = seq.slice(offset + start, offset + end)

  def numBases = length

  def count(base: DNA, start: Long, end: Long) = seq.count(base, offset + start, offset + end)

  /**
   * Count the number of occurrences of A, C, G and T letters within [start, end) at the same time.
   * This method is faster than repeating fastCount for each base.
   * @param start
   * @param end
   * @return
   */
  def count(start: Long, end: Long) = seq.count(offset + start, offset + end)

  /**
   * Create an materialized instance of this wrapped sequence
   * @return
   */
  protected def materialize = seq.slice(offset, offset+length)

  /**
   * Take the complement (not reversed) of this sequence
   * @return
   */
  def complement = materialize.complement

  /**
   * Create a reverse string of the this sequence. For example ACGT becomes TGCA
   *
   * @return Reverse sequence. The returned sequence is NOT a complement of
   *         the original sequence.
   */
  def reverse = materialize.reverse

  /**
   * Reverse complement of this sequence.
   * @return
   */
  def reverseComplement = materialize.reverseComplement
}