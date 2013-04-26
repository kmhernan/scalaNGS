/**
 * License
 *
 */

package com.kmh.ngs.fastq

import com.kmh.ngs.io
import java.io._

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
    
  // Getters
  def getReadHeader(): String = this.seqHeader
  def getSequence():  String = this.seqLine
  def getQualHeader(): String = this.qualHeader
  def getQuality(): String = this.qualLine
  def getBarcode(): String = this.seqHeader.split(":").toList.reverse.head

}

/**
 * Reads a fastq file
 *
 * @param reader instance of BufferedReader
 * @param file the file from which the buffer was created
 */
class FastqReader(val reader: BufferedReader, val file: File) {
  var nextRecord: FastqRecord = readNextRecord()

  def readNextRecord(): FastqRecord = {
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

  def hasNext(): Boolean = { nextRecord != null }

  def next(): FastqRecord = {
    val rec = nextRecord
    nextRecord = readNextRecord()
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
    val it = Iterator.continually { FQ.next() }
    try {
      for (rec <- it.takeWhile(_ != null)) yield { rec }
    }
  }

}
