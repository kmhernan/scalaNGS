/**
 * License
 *
 */

package com.scalangs.fastq

import com.scalangs.io
import java.io._

/**
 * Represents a literal Fastq record 
 *
 */

class FastqRecord(
        val seqHeader: String, 
        val seqLine: String, 
        val qualHeader: String, 
        val qualLine: String) {

  def getReadHeader(): String = this.seqHeader
  def getSequence():  String = this.seqLine
  def getQualHeader(): String = this.qualHeader
  def getQuality(): String = this.qualLine
}

/**
 * Reads a fastq file
 *
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

object FastqReader {
  def parseFastq(file: File): Iterator[FastqRecord] = {
    val rd = new com.scalangs.io.IoUtil
    val reader = rd.openFileForBufferedReading(file)
    val FQ = new FastqReader(reader, file)
    val it = Iterator.continually { FQ.next() }
    try {
      for (rec <- it.takeWhile(_ != null)) yield { rec }
    }
  }

}
