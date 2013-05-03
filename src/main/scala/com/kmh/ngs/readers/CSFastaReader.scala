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

package com.kmh.ngs
package readers
import java.io.{File, OutputStreamWriter, BufferedReader, IOException}
import org.eintr.loglady.Logging
import scala.collection.mutable.ListBuffer

/** 
 * Represents a Color-Space Fasta record
 *
 */
class CSFastaRecord(
	val seqHeader: String,
	val seqLine: String,
	val qualHeader: String,
	val qualLine: String) {

  /**
   * Returns the average quality of the current record
   * @return the average quality of the read.
   */
  def averageQuality: Double = {
    qualLine.split(" ").foldLeft(0)(_+_.toInt) / (seqLine.length - 1).toDouble
  }

  /**
   * Writes the current record to a file
   * @param ofa - file for csfasta reads
   * @param oq - file for quals
   *
   */ 
  def writeToFile(ofa: OutputStreamWriter, oq: OutputStreamWriter): Unit = {
    ofa.write(this.seqHeader + "\n")
    ofa.write(this.seqLine + "\n")
    oq.write(this.qualHeader + "\n")
    oq.write(this.qualLine + "\n")
  } 

}

/**
 * Reader class for CSFasta files 
 * @param seqReader - input stream for a csfasta file
 * @param qualReader - input stream for a qual file
 * @param seqFile - input csfasta file
 * @param qualFile - input quality file
 * @param start - optional 1-based index for trimming 5' end
 * @param end - optional 1-based index for trimming 3' end
 *
 */
class CSFastaReader(
  	val seqReader: BufferedReader, 
  	val qualReader: BufferedReader, 
	val seqFile: File,
	val qualFile: File,
        val start: Option[Int],
        val end: Option[Int]) extends Logging {
  var nextRecord: CSFastaRecord = readNextRecord

  /**
   * Reads next record from files 
   *
   * @return [[CSFastaRecord]]
   * @throws [[RuntimeException]] 
   * @throws [[IOException]] 
   */
  def readNextRecord: CSFastaRecord = {
    try {
      // Read Name
      val seqHeader: String = seqReader.readLine()
      if (seqHeader == null) return null
      if (seqHeader.isEmpty)
        log.error(throw new RuntimeException("Missing sequence header"))
      if (!seqHeader.startsWith(">"))
        log.error(throw new RuntimeException("Invalid sequence header type: Excepted '>'"))

      // Sequence
      val (seqLine, seqLength): (String, Int) = trimSeq(start, end, seqReader.readLine())

      // Qual Name 
      val qualHeader: String = qualReader.readLine()
      if (qualHeader != seqHeader)
        log.error(throw new RuntimeException(
          "Read quality header != read sequence header:\n%s\n%s".format(seqHeader, qualHeader)))
      if (!qualHeader.startsWith(">"))
        log.error(throw new RuntimeException("Invalid quality header type: Expected '>'"))
     
      // Quality
      val (qualLine, qualLength): (String, Int) = trimQ(start, end, qualReader.readLine())

      // Check lengths
      if (seqLength != qualLength){
        log.warn("Read %s has sequence length %s and quality length %s. Skipping read...".format(
		seqHeader, seqLength, qualLength))
        next
      }
      else 
        // Create new CSFastaRecord
        new CSFastaRecord(seqHeader, seqLine, qualHeader, qualLine)
    }
    catch {
      case ioe: IOException => log.error(
	"Error reading csfasta and quality '%s' '%s'".format(
		seqFile.getName(), qualFile.getName(), ioe));
    	seqReader.close();
	qualReader.close();
 	sys.exit(1)
    }
  }

  /**
   * Checks if there is a next record
   *
   */
  def hasNext: Boolean = { nextRecord != null }

  /**
   * Returns next record
   *
   */ 
  def next: CSFastaRecord = {
    val rec = nextRecord
    nextRecord = readNextRecord
    return rec
  }

  /**
   * If not truncating from the beginning, you have to convert cs 
   * to bs and create a new template_B
   * @param seq - the CS sequence as a string
   * @param st - the user-requested start position
   * @return template_B character
   */
  lazy val bsMap = Map('A' -> Array('A', 'C', 'G', 'T'),
                       'C' -> Array('C', 'A', 'T', 'G'),
		       'G' -> Array('G', 'T', 'A', 'C'),
		       'T' -> Array('T', 'G', 'C', 'A'))

  def csToBS(seq: String, st: Int): Char = {
    val bsArr = new ListBuffer[Char]
    try {
      for (i <- 0 until st) {
        if (i == 0)
          bsArr += seq(i)
        else {
          val curr_b = bsMap(bsArr(i-1))(seq(i).toString.toInt)
          bsArr += curr_b
        }
      }
    }
    catch {
      case npe: NullPointerException => log.error("You didn't initialize the array" + npe)
      case nse: NoSuchElementException => return '.' 
      case iob: IndexOutOfBoundsException => log.error("Error, not in bounds of ListBuffer "+iob) 
    }
    bsArr.last
  }

  /**
   * Trims CS sequence based on user-input
   * @param st - Optional start position (1-based)
   * @param en - Optional end position (1-based)
   * @param string - Sequence to trim
   *
   */
  def trimSeq(st: Option[Int], en: Option[Int], string: String): (String, Int) = {
    val currLen: Int = string.length - 1
    (st, en) match {
      case (Some(st), Some(en)) => 
	if (st == 1)
     	  (string.slice(st-1, en + 1), currLen)
        else {
  	  val adaptBase = csToBS(string, st)
          (adaptBase + string.slice(st, en + 1), currLen)
	}
      case (Some(st), None) =>
	if (st == 1)
	  (string, currLen) 
     	else {
 	  val adaptBase = csToBS(string, st) 
	  (adaptBase + string.slice(st, string.length), currLen)
	}
      case (None, Some(en)) =>
	(string.slice(0, string.length - en), currLen)
      case (None, None) => (string, currLen)
    }
  }

  /**
   * @method trimQ - Trims quality based on user input
   *
   */
  def trimQ(st: Option[Int], en: Option[Int], string: String): (String, Int) = {
    val currLen: Int = string.split(" ").length
    (st, en) match {
      case (Some(st), Some(en)) => 
     	  (string.split(" ").toArray.slice(st-1, en).mkString(" "), currLen)
      case (Some(st), None) =>
	  (string.split(" ").toArray.slice(st-1, string.length).mkString(" "), currLen)
      case (None, Some(en)) =>
	(string.split(" ").toArray.slice(0, string.length - en).mkString(" "), currLen)
      case (None, None) => (string, currLen)
    }
  }

}

/**
 * @object CSFastaReader - Companion object for CSFastaReader class
 *
 */
object CSFastaReader {

  def parseCSFasta(
	seqReader: BufferedReader, 
	qualReader: BufferedReader,
	seqFile: File,
	qualFile: File,
        start: Option[Int],
        end: Option[Int]): Iterator[CSFastaRecord] = {
    val CSFA = new CSFastaReader(seqReader, qualReader, 
		                 seqFile, qualFile,
				 start, end)
    val it = Iterator.continually { CSFA.next }
    for (rec <- it.takeWhile(_ != null)) yield { rec }
  }

}
