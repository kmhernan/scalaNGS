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
   * @method averageQuality - returns the average quality of the current record
   * @return the average quality of the read.
   */
  def averageQuality: Double = {
    val qualArray = this.qualLine.split(" ").map(_.toDouble).toArray
    qualArray.sum / qualArray.length
  }

  /**
   * @method isHomopolyer - tests whether the current read is a homopolymer based on user cutoff
   * @param polyLimit - the cutoff length to consider homopolymer, represented as a proportion of
			total read length
   * @return true if is a homopolymer else false
   *
   */
  def isHomopolymer(polyLimit: Double) = {
    val string = "0" * (this.seqLine.length * polyLimit).toInt
    if (this.seqLine.contains(string)) true else false
  } 
 
  /**
   * @method writeToFile - writes the current record to a file
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
 * @class CSFastaReader - reader class for CSFasta files 
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
        val end: Option[Int]) {
  var nextRecord: CSFastaRecord = readNextRecord

  /**
   * @method readNextRecord - Reads next record from files 
   * 
   */
  def readNextRecord: CSFastaRecord = {
    try {
      // Read Name
      val seqHeader: String = seqReader.readLine()
      if (seqHeader == null) return null
      if (seqHeader.isEmpty)
        throw new RuntimeException("Missing sequence header")
      if (!seqHeader.startsWith(">"))
        throw new RuntimeException("Invalid sequence header type: Excepted '>'")
      // Qual Name 
      val qualHeader: String = qualReader.readLine()
      if (qualHeader != seqHeader)
        throw new RuntimeException(
          "Read quality header != read sequence header:\n%s\n%s".format(seqHeader, qualHeader))
      if (!qualHeader.startsWith(">"))
        throw new RuntimeException("Invalid quality header type: Expected '>'")
      
      // Sequence
      val seqLine: String = seqReader.readLine()
     
      // Quality
      val qualLine: String = qualReader.readLine()

      // Trim if necessary
      val trSeq = trimSeq(start, end, seqLine)
      val trQual = trimQ(start, end, qualLine)
 
      new CSFastaRecord(seqHeader, trSeq, qualHeader, trQual)
    }
    catch {
      case ioe: IOException => println(
	"Error reading csfasta and quality '%s' '%s'".format(
		seqFile.getName(), qualFile.getName(), ioe));
    	seqReader.close();
	qualReader.close();
 	sys.exit(1)
    }
  }

  /**
   * @method hasNext - Checks if there is a next record
   *
   */
  def hasNext: Boolean = { nextRecord != null }

  /**
   * @method next - Returns next record
   *
   */ 
  def next: CSFastaRecord = {
    val rec = nextRecord
    nextRecord = readNextRecord
    return rec
  }

  /**
   * @method trimSeq - Trims sequence based on user-input
   *
   */
  def trimSeq(st: Option[Int], en: Option[Int], string: String): String = {
    (st, en) match {
      case (Some(st), Some(en)) => 
	if (st == 1)
     	  string.slice(st-1, en + 1)
        else {
  	  val adaptBase = csToBS(string, st)
          adaptBase + string.slice(st, en + 1)
	}
      case (Some(st), None) =>
	if (st == 1)
	  string 
     	else {
 	  val adaptBase = csToBS(string, st) 
	  adaptBase + string.slice(st, string.length)
	}
      case (None, Some(en)) =>
	string.slice(0, string.length - en)
      case (None, None) => string
    }
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
      case npe: NullPointerException => println("You didn't initialize the array" + npe)
      case nse: NoSuchElementException => return '.' 
      case iob: IndexOutOfBoundsException => println(" "+iob) 
    }
    bsArr.last
  }

  /**
   * @method trimQ - Trims quality based on user input
   *
   */
  def trimQ(st: Option[Int], en: Option[Int], string: String): String = {
    (st, en) match {
      case (Some(st), Some(en)) => 
     	  string.split(" ").toArray.slice(st-1, en).mkString(" ")
      case (Some(st), None) =>
	  string.split(" ").toArray.slice(st-1, string.length).mkString(" ")
      case (None, Some(en)) =>
	string.split(" ").toArray.slice(0, string.length - en).mkString(" ")
      case (None, None) => string
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
