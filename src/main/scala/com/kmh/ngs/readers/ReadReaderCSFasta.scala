package com.kmh.ngs.readers
import com.kmh.ngs.formats.{Read, CSFastaRecord}
import java.io.{File, BufferedReader, IOException}
import org.eintr.loglady.Logging
import scala.collection.mutable.ListBuffer

case class CSFastaReader(
	val seqReader: BufferedReader,
   	val qualReader: BufferedReader,
	val seqFile: File,
	val qualFile: File,
        val start: Option[Int],
        val end: Option[Int]) extends ReadReader with Logging {
  var nextRecord: CSFastaRecord = readNextRecord

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
   * Trims quality based on user input
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

  def hasNext: Boolean = { nextRecord != null }

  def next: CSFastaRecord = {
    val rec = nextRecord
    nextRecord = readNextRecord
    return rec
  }

  def iter: Iterator[CSFastaRecord] = {
    val it = Iterator.continually { this.next }
    for (rec <- it.takeWhile(_ != null)) yield { rec }
  }
}
