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

package com.kmh.ngs.tools

import com.kmh.ngs.readers.{ReadReader, FastqReader}
import com.kmh.ngs.formats.Read
import com.kmh.ngs.analyses.{ReadStatsByIndex, ReadIndexData}

import java.io.{File, BufferedReader, OutputStreamWriter}
import scala.collection.mutable

import org.eintr.loglady.Logging



/**
 * Creates summary statistics and figures for NGS sequence data.
 * This application was motivated by my aggravation with FASTX and
 * adapts similar code algorithms <http://hannonlab.cshl.edu/fastx_toolkit/> 
 * 
 * @constructor args a list of command-line arguments
 */
class ReadStatistics(val args: List[String]) extends NGSApp with Logging {
  def toolName = "'%s'".format(this.getClass()) 
  def description = "Calculates summary statistics for NGS reads "+
                    "and plots quality/base distribution. [GNUplot must be installed]."
  val SP = " " * ("usage: java -jar NGSTools.jar ".length)

  def checkMutuallyExclusiveArgs(map: OptionMap): Boolean = { 
    if (map.isDefinedAt('infq) && !map.isDefinedAt('instat)) true
    else if (map.isDefinedAt('instat) && !map.isDefinedAt('ostat)) true
    else if (map.isDefinedAt('instat) && map.isDefinedAt('offset)) {
      log.warn("Offset is meaningless when input is a stats file...")
      true
    } 
    else false
  }

  def checkRequired(map: OptionMap): OptionMap = {
    if (map.isEmpty) {
      mainUsage
      sys.exit(0)
    }
    else if (checkMutuallyExclusiveArgs(map)) {
      if (map.isDefinedAt('infq) && !map.isDefinedAt('offset)) {
        mainUsage
        log.error(throw new IllegalArgumentException("When input is a Fastq file, you must declare the offset!!"))
        sys.exit(1)
      }
      else if (map.isDefinedAt('instat) && !map.isDefinedAt('qualPlot) && !map.isDefinedAt('basePlot)) {
        mainUsage
        log.error(throw new IllegalArgumentException("When input is a stats file, you must declare plot options!!"))
        sys.exit(1)
      }
      else
        map
    } 
    else {
      mainUsage
      log.error(throw new IllegalArgumentException("Mutally Exclusive Arguments error!!"))
      sys.exit(1)
    }
  }

  def mainUsage = List(
    "usage: java -jar NGSTools.jar -T FilterReads {-INFQ file.fastq -QV-OFFSET [33,64] [-OSTAT file.txt] | -INSTAT file.txt}",
    SP+"[-BASEPLOT <file prefix>] [-QUALPLOT <file prefix>] [-h/--help]\n").map(println(_))

  def mainVerboseUsage = {
    mainUsage
    List("Mutually exclusive arguments:",
      "A. If input is a Fastq file, you can declare an output file for the results (default is stdout):",
      "  -INFQ \tREQUIRED: Input fastq file: <file.fastq> or <file.fastq.gz>",
      "  -OSTAT\tOPTIONAL: Output stats file: <file.txt> [default stdout]",
      "  -QV-OFFSET\tREQUIRED: Phred-scaled offset [33, 64]\n",
      "B. If input is a stats file produced from a previous run of this script, you can create plots:",
      "  -INSTAT\tREQUIRED: Input stats file: <file.txt>\n").map(println(_))
    List("Optional Arguments:",
      "  -BASEPLOT\tIf you want to produce a plot of base frequencies, place file prefix here "+
      "(e.g. /path/to/image/filename [no extension; automatically produces .png])",
      "           \tRequires GNUplot!",
      "  -QUALPLOT\tIf you want to produce a boxplot of quality scores, place file prefix here "+
      "(e.g. /path/to/image/filename [no extension; automatically produces .png])",
      "           \tRequires GNUplot!",
      "  -h/--help\tPrint this message and exit.\n").map(println(_))
  }

  def parse(map: OptionMap, list: List[String]): OptionMap = {
    list match {
      case Nil => checkRequired(map)
      // Mutually exclusive IO group A
      case "-INFQ" :: file :: tail => parse(map ++ Map('infq-> new File(file)), tail)
      case "-OSTAT" :: file :: tail => parse(map ++ Map('ostat-> new File(file)), tail)
      // Mututally exclusive IO group B
      case "-INSTAT" :: file :: tail => parse(map ++ Map('instat-> new File(file)), tail)
      // Other args
      case "-QV-OFFSET" :: value :: tail => parse(map ++ Map('offset->value.toInt), tail)
      case "-BASEPLOT" :: value :: tail => parse(map ++ Map('basePlot-> value), tail)
      case "-QUALPLOT" :: value :: tail => parse(map ++ Map('qualPlot-> value), tail)
      case "-h" :: tail => mainVerboseUsage; sys.exit(0)
      case "--help" :: tail => mainVerboseUsage; sys.exit(0)
      case option => mainUsage;
                     log.error(throw new IllegalArgumentException("Unknown Option "+option));
                     sys.exit(1);
    }
  }

  /**
   * Creates input stream, a list of output streams, and the instance of the read iterator.
   *
   * @param userOpts the map of the command-line arguments
   * @return The input stream
   * @return [[java.io.OutputStreamWriter]] the of output
   * @return [[com.kmh.ngs.readers.ReadReader]] an instance of a reader for NGS sequence files.
   */
  def loadReader(userOpts: OptionMap): (BufferedReader, Option[OutputStreamWriter], Option[ReadReader]) = {
    if (userOpts.isDefinedAt('infq) && userOpts.isDefinedAt('ostat)) {
      val inputFile    = userOpts('infq).asInstanceOf[File]
      val outputFile   = userOpts('ostat).asInstanceOf[File]
      ioInit.assertFileIsReadable(inputFile)
      val inputBuffer  = ioInit.openFileForBufferedReading(inputFile)
      val outputBuffer = ioInit.openFileForWriting(outputFile) 
      (inputBuffer, Some(outputBuffer), Some(new FastqReader(inputBuffer, inputFile, None, None))) 
    }

    else if (userOpts.isDefinedAt('infq) && !userOpts.isDefinedAt('ostat)) {
      val inputFile    = userOpts('infq).asInstanceOf[File]
      ioInit.assertFileIsReadable(inputFile)
      val inputBuffer  = ioInit.openFileForBufferedReading(inputFile)
      (inputBuffer, None, Some(new FastqReader(inputBuffer, inputFile, None, None))) 
    }

    else if (userOpts.isDefinedAt('instat)) {
      val inputFile    = userOpts('instat).asInstanceOf[File]
      ioInit.assertFileIsReadable(inputFile)
      val inputBuffer  = ioInit.openFileForBufferedReading(inputFile)
      (inputBuffer, None, None) 
    }
    // Should never happen 
    else {
      log.error(throw new RuntimeException("Something is very wrong!"))
      sys.exit(1)
    }
  }

  def processResults(results: Array[ReadIndexData], 
  	output: Option[OutputStreamWriter], 
	userOpts: OptionMap): Unit = {
    val ReadStatsHeader = 
   	Array[String]("Index", "N", "MinQ", "MaxQ", "SumQ",
                      "MeanQ", "Q1", "Median", "Q3", "IQR",
                      "A_ct", "C_ct", "G_ct", "T_ct", "N_ct").mkString("\t")
    output match {
      case Some(output) => {
        output.write(ReadStatsHeader + "\n")
        results.toArray.view.zipWithIndex.foreach {
          case(v, i) => {
            output.write("%s\t%s\t%s\t%s\t%s\t".format(i, v.counts, v.min, v.max, v.sum) +
                         "%2.2f\t%s\t%s\t%s\t%s\t".format(v.mean, v.q1, v.med, v.q3, v.iqr) +
                         "%s\t%s\t%s\t%s\t%s".format(v.nA, v.nC, v.nG, v.nT, v.nN) + "\n")
          }
        }
      }
      case None => {
        println(ReadStatsHeader)
        results.toArray.view.zipWithIndex.foreach {
          case(v, i) => {
            println("%s\t%s\t%s\t%s\t%s\t".format(i, v.counts, v.min, v.max, v.sum) +
                    "%2.2f\t%s\t%s\t%s\t%s\t".format(v.mean, v.q1, v.med, v.q3, v.iqr) +
                    "%s\t%s\t%s\t%s\t%s".format(v.nA, v.nC, v.nG, v.nT, v.nN))
          }
        }
      }
    }
  }
         
  /**
   * The main function for filtering reads. 
   * 
   * @throws [[IllegalArgumentException]]
   */ 
  def run = {
    val userOpts = parse(Map(), args) 
    loadReader(userOpts) match {
      case (inputBuffer, Some(outputBuffer), Some(readReader)) =>
        try
          processResults(
            ReadStatsByIndex(readReader, userOpts('offset).asInstanceOf[Int]),
            Some(outputBuffer),
            userOpts)
        catch {
          case err: Throwable => List(inputBuffer, outputBuffer).map(ioInit.closer(_)); 
            log.error(throw new Exception(err)); sys.exit(1)
        } finally List(inputBuffer, outputBuffer).map(ioInit.closer(_))
      case (inputBuffer, None, Some(readReader)) =>
        try
          processResults(
	    ReadStatsByIndex(readReader, userOpts('offset).asInstanceOf[Int]),
            None,
            userOpts)
        catch {
          case err: Throwable => ioInit.closer(inputBuffer); 
            log.error(throw new Exception(err)); sys.exit(1)
        } finally ioInit.closer(inputBuffer)
       case (inputBuffer, None, None) =>
         println("Ok");
         ioInit.closer(inputBuffer)
       // Should never happen
       case _ =>
         log.error(throw new Exception("Something is very wrong")); sys.exit(1)
    }
  } 

}
