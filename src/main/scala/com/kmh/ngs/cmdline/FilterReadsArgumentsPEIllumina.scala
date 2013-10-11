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
    SP+"{-O1/-OUTREAD1 file_R1.fastq -O2/-OUTREAD2 file_R2.fastq | -OUT-INTER file_R1_R2.fastq} ",
    SP+"{[-R1-TRIM-5 Int] [-R1-TRIM-3 Int] [-R2-TRIM-5 Int] [-R2-TRIM-3 Int] | ",
    SP+" [-R1-CLIP-5 String] [-R1-CLIP-3 String] [-R2-CLIP-5 String] [-R2-CLIP-3 String] ",
    SP+" [--R1-KEEP-5] [--R1-KEEP-3] [--R2-KEEP-5] [--R2-KEEP-3] [-MIN-LENGTH Int]} ",
    SP+"-QV-OFFSET {33,64} [-HPOLY Double] [-MINQ Int] [-NMISSING Int] [-h/--help]\n").foreach(println(_))

  def mainVerboseUsage = {
    mainUsage
    List("Required Arguments:",
      "Supports both separated and interleaved paired-end Fastq files.",
      "If input Fastq files are separated (mate-pairs must be sorted in same order):",
      "  -R1/-READ1 <String>\tInput raw fastq file for first paired-end: <file_R1.fastq> or <file_R1.fastq.gz>",
      "  -R2/-READ2 <String>\tInput raw fastq file for second paired-end: <file_R2.fastq> or <file_R2.fastq.gz>\n",
      "If input Fastq file is interleaved (pair 1 must always be followed by its mate-pair 2):",
      "  -INTER <String>\tInput raw fastq file containing both pairs: <file_R1_R2.fastq> or <file_R1_R2.fastq.gz>\n",
      "Regardless of input format, reads can be written to either separated or interleaved fastq files:",
      "  -O1/-OUTPUT1 <String>\tOutput separated fastq file for first paired-end: <file_R1.fastq>",
      "  -O2/-OUTPUT2 <String>\tOutput separated fastq file for second paired-end: <file_R2.fastq>",
      "  -OUT-INTER <String>\tOutput interleaved fastq file: <file_R1_R2.fastq>\n",
      "  -QV-OFFSET <Int>\tPhred-scaled offset [33, 64]\n").foreach(println(_))

    List("Optional Arguments:\n",
      "I. Read Clipping Options - I assume these are Illumina paired-end reads where R1 is forward and R2 is ",
      "                           the reverse strand. Thus, when looking at the FASTQ file, R1 goes from 5'->3' ",
      "                           while R2 goes from 3'->5'.",
      "A. Trim by index:",
      "  -R1-TRIM-5   " + "5' cut position for Read 1 (1-based index) ** Assumes R1 is forward strand **",
      "  -R1-TRIM-3   " + "3' cut position for Read 1 (1-based index) ** Assumes R1 is forward strand **",
      "  -R2-TRIM-5   " + "5' cut position for Read 2 (1-based index) ** Assumes R2 is reverse strand **",
      "  -R2-TRIM-3   " + "3' cut position for Read 2 (1-based index) ** Assumes R2 is reverse strand **",
      "B. Clip lead/tail by string match:",
      "  -R1-CLIP-5   " + "Searches for this sequence (IUPAC) and removes from the start of R1 to the end",
      "               " + "of the the search sequence. You can add the regex char '+'",
      "               " + "E.g., for the Juenger Lab's commonly used oligo: -R1-CLIP-5 NNMWGGG+",
      "  -R1-CLIP-3   " + "Searches for this sequence (IUPAC) and removes from the start of the search",
      "               " + "sequence to the end of R1. You can add the regex char '+'",
      "               " + "E.g., remove poly-A tail of some size from R1: -R1-CLIP-3 AAAAAA",
      "  -R2-CLIP-5   " + "Searches for this sequence (IUPAC) and removes from the 5' end of R2 to the end",
      "               " + "of the the search sequence. You can add the regex char '+'",
      "  -R2-CLIP-3   " + "Searches for this sequence (IUPAC) and removes from the 3' of the search",
      "               " + "sequence to the end of R1. You can add the regex char '+'",
      "               " + "E.g., remove poly-T tail of some size from R2: -R2-CLIP-3 TTTTTT",
      "  --R1-KEEP-5  " + "Keep reads that don't have the -R1-CLIP-5 sequence. Default, discarded.",
      "  --R1-KEEP-3  " + "Keep reads that don't have the -R1-CLIP-3 sequence. Default, discarded.",
      "               " + "** If you're trimming Poly-A tail, you will want to use this flag! **",
      "  --R2-KEEP-5  " + "Keep reads that don't have the -R2-CLIP-5 sequence. Default, discarded.",
      "  --R2-KEEP-3  " + "Keep reads that don't have the -R2-CLIP-3 sequence. Default, discarded.",
      "               " + "** If you're trimming Poly-T from R2, you will want to use this flag! **",
      "  -MIN-LENGTH  " + "Remove reads that are not >= this length after clipping.",
      
      "II. Read Filtering Options",
      "  -HPOLY       " + "Relative length of repetitive base to consider a homopolymer. ", 
      "               " + "(Proportion of read length; e.g., between 0 and 1)",
      "  -MINQ        " + "Minimum average quality score allowed.",
      "  -NMISSING    " + "Lower limit for N's allowed.",
      "  -h/--help    " + "Print this message and exit.\n").foreach(println(_))
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
      case "-R1-TRIM-5" :: value :: tail => parse(map ++ Map('r1Five->value.toInt), tail)
      case "-R1-TRIM-3" :: value :: tail => parse(map ++ Map('r1Three->value.toInt), tail)
      case "-R2-TRIM-5" :: value :: tail => parse(map ++ Map('r2Five->value.toInt), tail)
      case "-R2-TRIM-3" :: value :: tail => parse(map ++ Map('r2Three->value.toInt), tail)
      case "-R1-CLIP-5" :: value :: tail => parse(map ++ Map('r1ClipFive->value), tail)
      case "-R1-CLIP-3" :: value :: tail => parse(map ++ Map('r1ClipThree->value), tail)
      case "-R2-CLIP-5" :: value :: tail => parse(map ++ Map('r2ClipFive->value), tail)
      case "-R2-CLIP-3" :: value :: tail => parse(map ++ Map('r2ClipThree->value), tail)
      case "--R1-KEEP-5" :: value :: tail => parse(map ++ Map('r1KeepFive->true), tail)
      case "--R1-KEEP-3" :: value :: tail => parse(map ++ Map('r1KeepThree->true), tail)
      case "--R2-KEEP-5" :: value :: tail => parse(map ++ Map('r2KeepFive->true), tail)
      case "--R2-KEEP-3" :: value :: tail => parse(map ++ Map('r2KeepThree->true), tail)
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

object FilterPEIlluminaArgs {
  type OptionMap = Map[Symbol, Any]

  def apply(args: List[String]): OptionMap = {
    val initPEIllumina = new FilterPEIlluminaArgs
    initPEIllumina.parse(Map(), args)
  }
}
