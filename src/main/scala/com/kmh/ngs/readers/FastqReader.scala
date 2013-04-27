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

package com.kmh.ngs.fastq
import com.kmh.ngs.io
import java.io.{File, BufferedReader, IOException}

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
class FastqReader(val reader: BufferedReader, val file: File) {
  var nextRecord: FastqRecord = readNextRecord

  def readNextRecord: FastqRecord = {
    try {
      // Header
      val seqHeader: String = reader.readLine()
      if (seqHeader == null) return null
      if (seqHeader.isEmpty)
        throw new RuntimeException("Missing sequence header")
      if (!seqHeader.startsWith("@"))
        throw new RuntimeException("Invalid sequence header type")
      
      // Sequence
      val seqLine: String = reader.readLine()

      // Quality Header
      val qualHeader: String = reader.readLine()
      if (!qualHeader.startsWith("+"))
        throw new RuntimeException("Invalid quality header type")

      // Quality
      val qualLine: String = reader.readLine()

      // Check if sequence and quality are same lengths
      if (seqLine.length != qualLine.length)
        throw new RuntimeException("Sequence length must match quality length")
     
      new FastqRecord(seqHeader, seqLine, qualHeader, qualLine)
    }
    catch {
      case ioe: IOException => println(String.format("Error reading fastq '%s' '%s'", file.getName(), ioe)); 
                               reader.close(); sys.exit(1);
    }
  }

  def hasNext: Boolean = { nextRecord != null }

  def next: FastqRecord = {
    val rec = nextRecord
    nextRecord = readNextRecord
    return rec
  }

}

/**
 * Companion object for FastqReader class
 *
 */
object FastqReader {

  def parseFastq(file: File): Iterator[FastqRecord] = {
    val rd = new com.kmh.ngs.io.IoUtil
    val reader = rd.openFileForBufferedReading(file)
    val FQ = new FastqReader(reader, file)
    val it = Iterator.continually { FQ.next }
    try {
      for (rec <- it.takeWhile(_ != null)) yield { rec }
    }
  }

}
