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

class FilterPEIlluminaArgs extends Arguments with Logging {
  val SP = " " * ("Usage: java -jar NGSTools.jar ".length)
  val basesArray = Array[String]("A", "C", "G", "T")
  val required = List[Symbol]()
  def mainUsage = List(
    "usage: java -jar NGSTools.jar -T FilterReads -P/-PLATFORM PE_illumina ",
    SP+"{-R1/-READ1 file_R1.fastq -R2/-READ2 file_R2.fastq | -INTER file_R1_R2.fastq} ",
    SP+"{-O1/-OUTREAD1 file_R1.fastq -O2/-OUTREAD2 file_R2.fastq | -OUT-INTER file_R1_R2.fastq}",
    SP+"-QV-OFFSET {33, 64} [-START Int] [-END Int] [-HPOLY Double] [-MINQ Int] [-NMISSING Int]",
    SP+"[-POLYA Double Int] [-h/--help]\n").map(println(_))

  def mainVerboseUsage = {
    mainUsage
    List("Required Arguments:",
      "Supports both separated and interleaved paired-end Fastq files.",
      "If input Fastq files are separated (mate-pairs must be sorted in same order):",
      "  -R1/-READ1\tInput raw fastq file for first paired-end: <file_R1.fastq> or <file_R1.fastq.gz>",
      "  -R2/-READ2\tInput raw fastq file for second paired-end: <file_R2.fastq> or <file_R2.fastq.gz>\n",
      "If input Fastq file is interleaved (pair 1 must always be followed by its mate-pair 2):",
      "  -INTER\tInput raw fastq file containing both pairs: <file_R1_R2.fastq> or <file_R1_R2.fastq.gz>\n",
      "Regardless of input format, reads can be written to either separated or interleaved fastq files:",
      "  -O1/-OUTPUT1\tOutput separated fastq file for first paired-end: <file_R1.fastq>",
      "  -O2/-OUTPUT2\tOutput separated fastq file for second paired-end: <file_R2.fastq>",
      "  -OUT-INTER\tOutput interleaved fastq file: <file_R1_R2.fastq>\n",
      "  -QV-OFFSET\tPhred-scaled offset [33, 64]\n").map(println(_))
    List("Optional Arguments:",
      "  -START\t5' cut position (1-based index)",
      "  -END\t\t3' cut position (1-based index)",
      "      \t\tex. AlfI: -START 1 -END 36",
      "  -HPOLY\tRelative length of repetitive base to consider a homopolymer. (Proportion of read length; e.g., between 0 and 1)",
      "  -MINQ\t\tMinimum average quality score allowed.",
      "  -NMISSING\tLower limit for N's allowed.",
      "  -POLYA\tTakes two values:",
      "        \t  1) ProportionLimit [Double] - If a read has trailing A's of length <value> * sequence length, trim them.",
      "        \t  2) MinimumSize [Int] - If the trimmed sequence is shorter than <value>, remove it.",
      "  -h/--help\tPrint this message and exit.\n").map(println(_))
  }

  def checkRequired(map: OptionMap): OptionMap = {
    if (map.isDefinedAt('offset)) {
      if (map.isDefinedAt('inR1) && map.isDefinedAt('inR2) && !map.isDefinedAt('inInter)) {
        if (map.isDefinedAt('outR1) && map.isDefinedAt('outR2) && !map.isDefinedAt('outInter)) 
          map
        else if (!map.isDefinedAt('outR1) && !map.isDefinedAt('outR2) && map.isDefinedAt('outInter)) 
          map
        else {
          mainUsage
          log.error(throw new IllegalArgumentException("Can't output both interleaved and separated"))
          sys.exit(1)
         }
      }
      else if (map.isDefinedAt('inInter) && !map.isDefinedAt('inR1) && !map.isDefinedAt('inR2)) { 
        if (map.isDefinedAt('outInter) && !map.isDefinedAt('outR1) && !map.isDefinedAt('outR2))
          map
        else if (!map.isDefinedAt('outInter) && map.isDefinedAt('outR1) && map.isDefinedAt('outR2))
          map
        else {
          mainUsage
          log.error(throw new IllegalArgumentException("Can't output both interleaved and separated"))
          sys.exit(1)
         }
      }
      else {
        mainUsage
        log.error(throw new IllegalArgumentException("Missing Required Arguments!!"))
        sys.exit(1)
      }
    }
    else if (map.isEmpty) {
      mainUsage
      sys.exit(0)
    } 
    else {
      mainUsage
      log.error(throw new IllegalArgumentException("Missing Required Arguments!!"))
      sys.exit(1)
    }
  }

  def parse(map: OptionMap, list: List[String]): OptionMap = {
    list match {
      case Nil => checkRequired(map)
      case "-R1" :: file :: tail => parse(map ++ Map('inR1-> new File(file)), tail)
      case "-READ1" :: file :: tail => parse(map ++ Map('inR1-> new File(file)), tail)
      case "-R2" :: file :: tail => parse(map ++ Map('inR2-> new File(file)), tail)
      case "-READ2" :: file :: tail => parse(map ++ Map('inR2-> new File(file)), tail)
      case "-O1" :: file :: tail => parse(map ++ Map('outR1-> new File(file)), tail)
      case "-OUTPUT1" :: file :: tail => parse(map ++ Map('outR1-> new File(file)), tail)
      case "-O2" :: file :: tail => parse(map ++ Map('outR2-> new File(file)), tail)
      case "-OUTPUT2" :: file :: tail => parse(map ++ Map('outR2-> new File(file)), tail)
      case "-INTER" :: file :: tail => parse(map ++ Map('inInter-> new File(file)), tail)
      case "-OUT-INTER" :: file :: tail => parse(map ++ Map('outInter-> new File(file)), tail)
      case "-QV-OFFSET" :: value :: tail => parse(map ++ Map('offset->value.toInt), tail)
      case "-START" :: value :: tail => parse(map ++ Map('start->value.toInt), tail)
      case "-END" :: value :: tail => parse(map ++ Map('end->value.toInt), tail)
      case "-HPOLY" :: value :: tail => parse(map ++ Map('hpoly->value.toDouble), tail)
      case "-MINQ" :: value :: tail => parse(map ++ Map('minq->value.toInt), tail)
      case "-NMISSING" :: value :: tail => parse(map ++ Map('minN->value.toInt), tail)
      case "-POLYA" :: value :: value2 :: tail =>
        try
          parse(map ++ Map('polyA->value.toDouble, 'minSize->value2.toInt), tail)
        catch {
          case err: Throwable =>
                log.error("POLYA takes 2 values: ProportionLimit [Double] MinimumSize [Int]"+err);
                sys.exit(1)
        }
      case "-h" :: tail => mainVerboseUsage; sys.exit(0)
      case "--help" :: tail => mainVerboseUsage; sys.exit(0)
      case option => mainUsage;
                     log.error(throw new IllegalArgumentException("Unknown Option "+option));
                     sys.exit(1);
    }
  }

}

object FilterPEIlluminaArgs {
  type OptionMap = Map[Symbol, Any]

  def apply(args: List[String]): OptionMap = {
    val initPEIllumina = new FilterPEIlluminaArgs
    initPEIllumina.parse(Map(), args)
  }
}
