package com.kmh.ngs.readers
import com.kmh.ngs.formats._
import java.io.{File, BufferedReader, IOException}
import org.eintr.loglady.Logging

trait ReadReader extends Read {
  var nextRecord: Read = readNextRecord
  def readNextRecord: Read
  def hasNext: Boolean
  def next: Read
}

case class CSFastaReader(
	val seqReader: BufferedReader,
   	val qualReader: BufferedReader
	val seqFile: File,
	val qualFile: File) extends ReadReader with Logging {
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
      seqLine: String = seqReader.readLine()

      // Qual Name
      val qualHeader: String = qualReader.readLine()

      if (qualHeader != seqHeader)
        log.error(throw new RuntimeException(
          "Read quality header != read sequence header:\n%s\n%s".format(seqHeader, qualHeader)))
      if (!qualHeader.startsWith(">"))
        log.error(throw new RuntimeException("Invalid quality header type: Expected '>'"))

      // Quality
      val qualLine: String = qualReader.readLine()

      // Check lengths
      if (seqLine.length - 1 != qualLine.split(" ").length){
        log.warn("Read %s has sequence length %s and quality length %s. Skipping read...".format(
                seqHeader, seqLine.length, qualLine.split(" ").length))
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

  def hasNext: Boolean = { nextRecord != null }

  def next: CSFastaRecord = {
    val rec = nextRecord
    nextRecord = readNextRecord
    return rec
  }
}

object InitReader {
  def apply(
	seqReader: BufferedReader,
	qualReader: BufferedReader,
	seqFile: File,
	qualFile: File,
	platform: String) = {
  platform match {
    case "solid" => {
      val CSFA = new CSFastaReader(seqReader, qualReader, seqFile, qualFile)
      val it = Iterator.continually { CSFA.next }
      for (rev <- it.takeWhile(_ != null)) yield { rec }
  }
}
