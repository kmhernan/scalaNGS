package com.kmh.ngs.csfasta
import java.io._
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

  def averageQuality = {
    val qualArray = this.qualLine.split(" ").map(_.toDouble).toArray
    qualArray.sum / qualArray.length
  }

  def isHomopolymer(polyLimit: Double) = {
    val string = "0" * (this.seqLine.length * polyLimit).toInt
    if (this.seqLine.contains(string)) true else false
  } 
  
  def writeToFile(ofa: OutputStreamWriter, oq: OutputStreamWriter): Unit = {
    ofa.write(this.seqHeader + "\n")
    ofa.write(this.seqLine + "\n")
    oq.write(this.qualHeader + "\n")
    oq.write(this.qualLine + "\n")
  } 
}

/**
 * Class with parsing tools
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
   * Reads next record from files and does some
   * checks
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
   * Trims sequence based on user-input
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
   * Trims quality based on user input
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
 * Companion object for CSFastaReader class
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
