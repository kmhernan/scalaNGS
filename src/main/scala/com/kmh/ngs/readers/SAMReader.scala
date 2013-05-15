package com.kmh.ngs.readers

import com.kmh.ngs.formats.SAMRecord
import java.io.{File, BufferedReader, IOException}
import org.eintr.loglady.Logging
import scala.collection.mutable.ListBuffer

class SAMReader(
	val samReader: BufferedReader,
	val samFile: File) extends Logging {
  var nextRecord: SAMRecord = readNextRecord

  def readNextRecord: SAMRecord = {
    try {
      val currLine = samReader.readLine()
      if (currLine.startsWith("@HD")) {
        if (currLine.split("\t")(1).split(":")(1) != "1.4") {
          throw new RuntimeException("Requires SAM format v1.4!! '%s'".format(currLine))
          null
        }
        else
          next
      }

      if (currLine.startsWith("@"))
        next

      else { 
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
      case ioe: IOException => log.error(throw new IOException(
        "Error reading SAM file '%s'\n".format(samFile.getName()+ioe)));
        samReader.close();
        sys.exit(1)
    }
  }

  def hasNext: Boolean = { nextRecord != null }

  def next: SAMRecord = {
    val rec = nextRecord
    nextRecord = readNextRecord
    return rec
  }

  def iter: Iterator[CSFastaRecord] = {
    val it = Iterator.continually { this.next }
    for (rec <- it.takeWhile(_ != null)) yield { rec }
  }
}
