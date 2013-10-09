package com.kmh.ngs.analyses
import com.kmh.ngs.readers.PEFastqReader
import org.eintr.loglady.Logging
import com.kmh.ngs.formats.{Read, PEFastqRecord}
import java.io._

object GetPairAlignments extends Logging {

  val rev = Map[Char, Char]('A' -> 'T', 'G' -> 'C', 'T' -> 'A', 'C' -> 'G', 'N' -> '-')
  var total = 0
  var overlapping = 0
  var nonoverlapping = 0

  def apply(outputBuffer: List[OutputStreamWriter], reader: PEFastqReader, offset: Int): Unit = {
    reader.iter.foreach {x =>
      total += 1
      val aligner = SmithWaterman(x.sequence, x.read2.sequence.reverse.map(rev(_)).mkString)
      //if (aligner.computeSmithWaterman >= 400){ 
      println(aligner.computeSmithWaterman)
      println(aligner.alignments.mkString("\n"))
      //}
    }

    println("Total= %s\nOverlapping= %s\nNon-Overlapping= %s\nPercent Overlap= %.3f".format(
      total, overlapping, nonoverlapping, overlapping / nonoverlapping.toDouble))
  }

}
