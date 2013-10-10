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
import com.kmh.ngs.formats.{Read, FastqRecord, PEFastqRecord}
import java.io.{File, BufferedReader, IOException}
import org.eintr.loglady.Logging

case class FastqReader(
	val seqReader: BufferedReader,
	val seqFile: File) extends ReadReader with Logging {
  var nextRecord: FastqRecord = readNextRecord

  def readNextRecord: FastqRecord = {
    try {
      // Header
      val seqHeader: String = seqReader.readLine()
      if (seqHeader == null) return null
      if (seqHeader.isEmpty)
        log.error(throw new RuntimeException("Missing sequence header"))
      if (!seqHeader.startsWith("@"))
        log.error(throw new RuntimeException("Invalid sequence header type"))

      // Sequence
      val seqLine: String = seqReader.readLine()

      // Quality Header
      val qualHeader: String = seqReader.readLine()
      if (!qualHeader.startsWith("+"))
        log.error(throw new RuntimeException("Invalid quality header type"))

      // Quality
      val qualLine: String = seqReader.readLine()

      // Check if sequence and quality are same lengths
      if (seqLine.length != qualLine.length)
        log.error(throw new RuntimeException("Sequence length must match quality length"))

      // Create instance of FastqRecord 
      new FastqRecord(seqHeader, seqLine, qualHeader, qualLine)
    }

    catch {
      case ioe: IOException => 
        log.error("Error reading fastq file '%s'".format(seqFile.getName(), ioe));
        seqReader.close();
        sys.exit(1)
    }
  }

  def hasNext: Boolean = { nextRecord != null }

  def next: FastqRecord = {
    val rec = nextRecord
    nextRecord = readNextRecord
    return rec
  }

  def iter: Iterator[FastqRecord] = {
    val it = Iterator.continually { this.next }
    for (rec <- it.takeWhile(_ != null)) yield { rec }
  }
}
