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

case class PEFastqReader(
	val seqReader: BufferedReader,
	val mateReader: Option[BufferedReader],
	val seqFile: File,
	val mateFile: Option[File]) extends ReadReader with Logging {
  var nextRecord: PEFastqRecord = readNextRecord

  def readNextRecord: PEFastqRecord = 
    (mateReader, mateFile) match {
      case (Some(mateReader), Some(mateFile)) =>
        try {
          // Header
          val r1Header: String = seqReader.readLine()
          val r2Header: String = mateReader.readLine()
          if (r1Header == null || r2Header == null) return null
          if (r1Header.isEmpty || r2Header.isEmpty)
            log.error(throw new RuntimeException("Missing sequence header"))
          if (!r1Header.startsWith("@") || !r2Header.startsWith("@"))
            log.error(throw new RuntimeException("Invalid sequence header type"))

          // Sequence
          val r1Line: String = seqReader.readLine()
          val r2Line: String = mateReader.readLine()

          // Quality Header
          val q1Header: String = seqReader.readLine()
          val q2Header: String = mateReader.readLine()
          if (!q1Header.startsWith("+") || !q2Header.startsWith("+"))
            log.error(throw new RuntimeException("Invalid quality header type"))

          // Quality
          val q1Line: String = seqReader.readLine()
          val q2Line: String = mateReader.readLine()

          // Check if sequence and quality are same lengths
          if (r1Line.length != q1Line.length || r2Line.length != q2Line.length)
            log.error(throw new RuntimeException("Sequence length must match quality length"))

          // Create instance of FastqRecord 
          new PEFastqRecord(r1Header, r1Line, q1Header, q1Line,
		            new FastqRecord(r2Header, r2Line, q2Header, q2Line))
        }
        catch {
          case ioe: IOException => 
            log.error("Error reading separated paired-end fastq files: '%s' '%s'".format(
              seqFile.getName(), Some(mateFile.getName())) + ioe);
            seqReader.close();
            mateReader.close();
            sys.exit(1)
        }
      case (None, None) =>
        try {
          // Header
          val r1Header: String = seqReader.readLine()
          if (r1Header == null) return null
          if (r1Header.isEmpty)
            log.error(throw new RuntimeException("Missing sequence header"))
          if (!r1Header.startsWith("@"))
            log.error(throw new RuntimeException("Invalid sequence header type"))
       
          // Sequence
          val r1Line: String = seqReader.readLine()

      	  // Quality Header
          val q1Header: String = seqReader.readLine()
          if (!q1Header.startsWith("+"))
            log.error(throw new RuntimeException("Invalid quality header type"))

          // Quality
          val q1Line: String = seqReader.readLine()
      
          // Read 2
          val r2Header: String = seqReader.readLine()
          if (!r2Header.startsWith("@"))
            log.error(throw new RuntimeException("Invalid sequence header type"))
       
          // Sequence
          val r2Line: String = seqReader.readLine()

          // Quality Header
          val q2Header: String = seqReader.readLine()
          if (!q2Header.startsWith("+"))
            log.error(throw new RuntimeException("Invalid quality header type"))

          // Quality
          val q2Line: String = seqReader.readLine()

          // Check if sequence and quality are same lengths
          if (r1Line.length != q1Line.length || r2Line.length != q2Line.length)
            log.error(throw new RuntimeException("Sequence length must match quality length"))

          // Create instance of FastqRecord 
          new PEFastqRecord(r1Header, r1Line, q1Header, q1Line,
		            new FastqRecord(r2Header, r2Line, q2Header, q2Line))
        }
        catch {
          case ioe: IOException => 
            log.error("Error reading interleaved paired-end fastq file: '%s' ".format(
              seqFile.getName()) + ioe);
            seqReader.close();
            sys.exit(1)
        }
      case option =>
        log.error(throw new Exception("Incompatible files"));
        seqReader.close();
        sys.exit(1)
      }

  def hasNext: Boolean = { nextRecord != null }

  def next: PEFastqRecord = {
    val rec = nextRecord
    nextRecord = readNextRecord
    return rec
  }

  def iter: Iterator[PEFastqRecord] = {
    val it = Iterator.continually { this.next }
    for (rec <- it.takeWhile(_ != null)) yield { rec }
  }
}
