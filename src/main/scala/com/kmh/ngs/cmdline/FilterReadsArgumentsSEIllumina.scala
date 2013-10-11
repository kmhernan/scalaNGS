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
 * For more information, please refer to <http://unlicense.org/>
 *
 */

package com.kmh.ngs.cmdline
import org.eintr.loglady.Logging
import java.io.File

class FilterSEIlluminaArgs extends Arguments with Logging {
  val SP = " " * ("usage: java -jar NGSTools.jar ".length)
  val required = List('infq, 'outfq, 'offset)
  val basesArray = Array[String]("A", "C", "G", "T")
 
  def mainUsage = List(
    "usage: java -jar NGSTools.jar -T FilterReads -P/-PLATFORM SE_illumina",
    SP+"-I/-INPUT file.fastq -O/-OUTPUT file.fastq -QV-OFFSET {33,64}",
    SP+"[-START Int] [-END Int] { [-CLIP-LEAD String] [-CLIP-TAIL String]",
    SP+"[-MIN-LENGTH Int] [--KEEP-LEAD] [--KEEP-TAIL]}",
    SP+"[-HPOLY Double] [-MINQ Int] [-NMISSING Int] [-h/--help]\n").foreach(println(_))

  def mainVerboseUsage = {
    mainUsage
    List("Required Arguments:",
      "  -I/-INPUT       " + "Input raw read files: <file.fastq> or <file.fastq.gz>",
      "  -O/-OUTPUT      " + "Output filtered read files: <file.fastq>",
      "  -QV-OFFSET      " + "Phred-scaled offset [33, 64]\n",
      "Optional Arguments:",
      "I. Read Clipping Options",
      "A. Trim by index:",
      "  -START          " + "5' cut position (1-based index).",
      "  -END            " + "3' cut position (1-based index). Ex. AlfI: -START 1 -END 36",
      "B. Clip lead by string match and/or clip tail by string match:",
      "  -CLIP-LEAD      " + "Searches for this sequence (IUPAC) and removes from the start of ",
      "                  " + "the read to the end of the search sequence. You can add in the regex char '+'",
      "                  " + "E.g., for the Juenger Lab's commonly used oligo: -CLIP-LEAD NNMWGGG+",
      "  -CLIP-TAIL      " + "Searches for this sequence (IUPAC) and removes from the beginning of ",
      "                  " + "the search sequence to the end of the read. You can add in the regex char '+'",
      "                  " + "E.g., to remove poly-A of some size: -CLIP-TAIL AAAAAA",
      "  -MIN-LENGTH     " + "Remove reads that are not >= this length after clipping",
      "  --KEEP-LEAD     " + "Keep reads that don't have the lead search sequence. Default, discarded.",
      "  --KEEP-TAIL     " + "Keep reads that don't have the tail search sequence. Default, discarded.",
      "                  " + "** If you are trimming poly-A you want to add this flag **",
      "II. Read Filtering Options", 
      "  -HPOLY          " + "Remove homopolymers based on the relative length of repetitive base ",
      "                  " + "to consider a homopolymer.", 
      "                  " + "(Proportion of read length - between 0.0 and 1.0).",
      "  -MINQ           " + "Minimum average quality score allowed (integer).",
      "  -NMISSING       " + "Lower limit for N's allowed.",
      "  -h/--help       " + "Print this message and exit.\n").foreach(println(_))
  }

  def checkRequired(map: OptionMap): OptionMap = {
    if (required.forall(x => map.isDefinedAt(x)))
      map
    else if (map.isEmpty) {
      mainUsage
      sys.exit(0)
    } else {
      mainUsage
      log.error(throw new IllegalArgumentException("Missing Required Arguments!!"))
      sys.exit(1)
    }
  }

  def parse(map: OptionMap, list: List[String]): OptionMap = {
    list match {
      case Nil => checkRequired(map)
      case "-I" :: file :: tail => parse(map ++ Map('infq-> new File(file)), tail)
      case "-INPUT" :: file :: tail => parse(map ++ Map('infq-> new File(file)), tail)
      case "-O" :: file :: tail => parse(map ++ Map('outfq-> new File(file)), tail)
      case "-OUTPUT" :: file :: tail => parse(map ++ Map('outfq-> new File(file)), tail)
      case "-QV-OFFSET" :: value :: tail => parse(map ++ Map('offset->value.toInt), tail)
      case "-START" :: value :: tail => parse(map ++ Map('start->value.toInt), tail)
      case "-END" :: value :: tail => parse(map ++ Map('end->value.toInt), tail)
      case "-CLIP-LEAD" :: value :: tail => parse(map ++ Map('clipLead->value), tail)
      case "--KEEP-LEAD" :: tail => parse(map ++ Map('keepLead->true), tail)
      case "-CLIP-TAIL" :: value :: tail => parse(map ++ Map('clipTail->value), tail)
      case "--KEEP-TAIL" :: tail => parse(map ++ Map('keepTail->true), tail)
      case "-MIN-LENGTH" :: value :: tail => parse(map ++ Map('minSize->value.toInt), tail)
      case "-HPOLY" :: value :: tail => parse(map ++ Map('hpoly->value.toDouble), tail)
      case "-MINQ" :: value :: tail => parse(map ++ Map('minq->value.toInt), tail)
      case "-NMISSING" :: value :: tail => parse(map ++ Map('minN->value.toInt), tail)
      case "-h" :: tail => mainVerboseUsage; sys.exit(0)
      case "--help" :: tail => mainVerboseUsage; sys.exit(0)
      case option => mainUsage;
                     log.error(throw new IllegalArgumentException("Unknown Option "+option));
                     sys.exit(1);
    }
  }

}

object FilterSEIlluminaArgs {
  type OptionMap = Map[Symbol, Any]

  def apply(args: List[String]): OptionMap = {
    val initIllumina = new FilterSEIlluminaArgs
    initIllumina.parse(Map(), args)
  }
}
