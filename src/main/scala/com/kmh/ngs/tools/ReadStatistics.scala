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
  def description = "Calculates summary statistics for NGS reads "
  val SP = " " * ("usage: java -jar NGSTools.jar ".length)

  def checkRequired(map: OptionMap): OptionMap = {
    if (map.isEmpty) {
      mainUsage
      sys.exit(0)
    }
    else if (map.isDefinedAt('infq) && !map.isDefinedAt('offset)) {
      mainUsage
      log.error(throw new IllegalArgumentException("When input is a Fastq file, you must declare the offset!!"))
      sys.exit(1)
    }
    else
      map
  }

  def mainUsage = List(
    "usage: java -jar NGSTools.jar -T FilterReads -INFQ file.fastq -QV-OFFSET [33,64] [-OSTAT file.txt]\n").foreach(println(_))

  def mainVerboseUsage = {
    mainUsage
    List("Required arguments:",
      "  -INFQ <String>\tInput fastq file: <file.fastq> or <file.fastq.gz>",
      "  -QV-OFFSET <Int>\tPhred-scaled offset [33, 64]\n").foreach(println(_))
    List("Optional Arguments:",
      "  -OSTAT <String>\tOutput stats file: <file.txt> [default stdout]",
      "  -h/--help\t\tPrint this message and exit.\n").foreach(println(_))
  }

  /**
   * Parses command-line options into [[com.kmh.ngs.NGSApp.OptionMap]]
   * 
   * @throws IllegalArgumentException
   */ 
  def parse(map: OptionMap, list: List[String]): OptionMap = {
    list match {
      case Nil => checkRequired(map)
      case "-INFQ" :: file :: tail => parse(map ++ Map('infq-> new File(file)), tail)
      case "-OSTAT" :: file :: tail => parse(map ++ Map('ostat-> new File(file)), tail)
      case "-QV-OFFSET" :: value :: tail => parse(map ++ Map('offset->value.toInt), tail)
      case "-h" :: tail => mainVerboseUsage; sys.exit(0)
      case "--help" :: tail => mainVerboseUsage; sys.exit(0)
      case option => mainUsage;
                     log.error(throw new IllegalArgumentException("Unknown Option "+option));
                     sys.exit(1);
    }
  }

  /**
   * Creates input stream, an optional output stream, and the instance of the read iterator.
   *
   * @param userOpts the map of the command-line arguments
   * @return The input stream
   * @return [[java.io.OutputStreamWriter]] the of output
   * @return [[com.kmh.ngs.readers.ReadReader]] an instance of a reader for NGS sequence files.
   * @throws RuntimeException
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
    // Should never happen 
    else {
      log.error(throw new RuntimeException("Something is very wrong!"))
      sys.exit(1)
    }
  }

  /**
   * IO wrapper from results of [[com.kmh.ngs.analses.ReadStatsByIndex]]
   * object.
   *
   * @param results contained in an [[Array[com.kmh.ngs.analyses.ReadStatsByIndex]]]
   * @param output the [[Option[java.io.OutputStreamWriter]]] to write stats file to.
   * @param userOpts the map containing command-line arguments.
   */
  def processResults(
	results: Array[ReadIndexData], 
  	output: Option[OutputStreamWriter], 
	userOpts: OptionMap): Unit = {
    val ReadStatsHeader = 
   	Array[String]("Index", "N", "Min", "Max", "Sum",
                      "Mean", "StdDev", "Median",
                      "A_ct", "C_ct", "G_ct", "T_ct", "N_ct").mkString("\t")
    output match {
      case Some(output) => {
        output.write(ReadStatsHeader + "\n")
        results.toArray.view.zipWithIndex.foreach {
          case(v, i) =>
            output.write("%s\t%s\t%s\t%s\t%s\t".format(i, v.counts, v.min, v.max, v.sum) +
                         "%2.2f\t%2.2f\t%s\t".format(v.mean, v.stdev, v.med) +
                         "%s\t%s\t%s\t%s\t%s".format(v.nA, v.nC, v.nG, v.nT, v.nN) + "\n")
        }
      }
      case None => {
        println(ReadStatsHeader)
        results.toArray.view.zipWithIndex.foreach {
          case(v, i) => {
            println("%s\t%s\t%s\t%s\t%s\t".format(i, v.counts, v.min, v.max, v.sum) +
                    "%2.2f\t%2.2f\t%s\t".format(v.mean, v.stdev, v.med) +
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
   * @throws [[RuntimeException]]
   * @throws [[Exception]]
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
          case err: Throwable => log.error(throw new Exception(err))
        } finally List(inputBuffer, outputBuffer).map(ioInit.closer(_))

      case (inputBuffer, None, Some(readReader)) =>
        try
          processResults(
	    ReadStatsByIndex(readReader, userOpts('offset).asInstanceOf[Int]),
            None,
            userOpts)
        catch {
          case err: Throwable => log.error(throw new Exception(err))
        } finally ioInit.closer(inputBuffer)

       case _ =>
         log.error(throw new Exception("Something is very wrong")); sys.exit(1)
    }
  } 

}
