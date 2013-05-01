/**
 * This is free and unencumbered software released into the public domain.
 *
 * Anyone is free to copy, modify, publish, use, compile, sell, or
 * distribute this software, either in source code form or as a compiled
 * binary, for any purpose, commercial or non-commercial, and by any
 * means.
 *
 * In jurisdictions that recognize copyright laws, the author or authors
 * of this software dedicate any and all copyright interest in the
 * software to the public domain. We make this dedication for the benefit
 * of the public at large and to the detriment of our heirs and
 * successors. We intend this dedication to be an overt act of
 * relinquishment in perpetuity of all present and future rights to this
 * software under copyright law.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
 * IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR
 * OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
 * ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
 * OTHER DEALINGS IN THE SOFTWARE.
 *
 *
 * For more information, please refer to <http://unlicense.org/>
 *
 */

package com.kmh.ngs.readers
import com.kmh.ngs.io
import java.io.{File, BufferedReader, IOException}
import org.eintr.loglady.Logging

/**
 * Represents a literal Fastq record 
 *
 * @param seqHeader the read name
 * @param seqLine the sequence
 * @param qualHeader the optional quality name
 * @param qualLine the quality
 */

class FastqRecord(
        val seqHeader: String, 
        val seqLine: String, 
        val qualHeader: String, 
        val qualLine: String) {

  /**
   * Get the average Phred-scaled quality score based on the read offset.
   *
   * @param qv_offset the quality offset (33/64)
   * @returns average quality [Double]
   */
  def averageQuality(qv_offset: Int): Double = {
    val qualArr: Array[Int] = this.qualLine.map(_.toInt - qv_offset).toArray
    qualArr.sum.toDouble / qualArr.length
  } 
    
  def barcode(): String = this.seqHeader.split(":").toList.reverse.head

}

/**
 * Reads a fastq file
 *
 * @param reader instance of BufferedReader
 * @param file the file from which the buffer was created
 */
class FastqReader(
	val reader: BufferedReader, 
	val file: File,
	val start: Option[Int],
	val end: Option[Int]) extends Logging {
  var nextRecord: FastqRecord = readNextRecord

  def readNextRecord: FastqRecord = {
    try {
      // Header
      val seqHeader: String = reader.readLine()
      if (seqHeader == null) return null
      if (seqHeader.isEmpty)
        log.error(throw new RuntimeException("Missing sequence header"))
      if (!seqHeader.startsWith("@"))
        log.error(throw new RuntimeException("Invalid sequence header type"))
      
      // Sequence
      val seqLine: String = trim(start, end, reader.readLine())

      // Quality Header
      val qualHeader: String = reader.readLine()
      if (!qualHeader.startsWith("+"))
        log.error(throw new RuntimeException("Invalid quality header type"))

      // Quality
      val qualLine: String = trim(start, end, reader.readLine())

      // Check if sequence and quality are same lengths
      if (seqLine.length != qualLine.length)
        log.error(throw new RuntimeException("Sequence length must match quality length"))
    
      // Create instance of FastqRecord 
      new FastqRecord(seqHeader, seqLine, qualHeader, qualLine)
    }
    catch {
      case ioe: IOException => log.error("Error reading fastq '%s' '%s'".format(file.getName(), ioe)); 
                               reader.close(); sys.exit(1);
    }
  }

  def hasNext: Boolean = { nextRecord != null }

  def next: FastqRecord = {
    val rec = nextRecord
    nextRecord = readNextRecord
    return rec
  }

  /**
   * Trims sequence and qual based on user-input
   * @param st - Optional start position (1-based index).
   * @param en - Optional end position (1-based index).
   * @param string - Either quality or sequence to trim.
   *
   */
  def trim(st: Option[Int], en: Option[Int], string: String): String = {
    (st, en) match {
      case (Some(st), Some(en)) => string.slice(st-1, en + 1)
      case (Some(st), None) => string.slice(st-1, string.length)
      case (None, Some(en)) => string.slice(0, en + 1)
      case (None, None) => string
    }
  }

}

/**
 * Object for reading fastq files 
 *
 */
object FastqReader {
  def parseFastq(
        seqReader: BufferedReader,
        seqFile: File,
        start: Option[Int],
        end: Option[Int]): Iterator[FastqRecord] = {
    val FQ = new FastqReader(seqReader, seqFile, start, end)
    val it = Iterator.continually { FQ.next }
    for (rec <- it.takeWhile(_ != null)) yield { rec }
  }
}
