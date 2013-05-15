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
import com.kmh.ngs.plotting.MultiQualBasesPlot

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

  /**
   * Test for correct usage of mutually exclusive arguments.
   *
   * @param map an [[com.kmh.ngs.NGSApp.OptionMap]] containing command-line arguments
   * @return true if passed, else false
   */
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
      else if (map.isDefinedAt('instat) && !map.isDefinedAt('plot)) {
        mainUsage
        log.error(throw new IllegalArgumentException("When input is a stats file, you must declare plot options!!"))
        sys.exit(1)
      } 
      else if (map.isDefinedAt('plot) && !map('plot).toString.endsWith(".png") && 
        !map('plot).toString.endsWith(".PNG")) {
        mainUsage
        log.error(throw new IllegalArgumentException("Plot file must end with '.png'" +
	  " otherwise it will be corrupt!!"))
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
    SP+"[-PLOT <file.png>] [-h/--help]\n").map(println(_))

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
      "  -PLOT\tIf you want to produce a multi-plot of base frequencies and quality score, "+
      "place path to file.png here (Must be .png)",
      "           \tRequires GNUplot! (try: gnuplot -V)",
      "  -h/--help\tPrint this message and exit.\n").map(println(_))
  }

  /**
   * Parses command-line options into [[com.kmh.ngs.NGSApp.OptionMap]]
   * 
   * @throws IllegalArgumentException
   */ 
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
      case "-PLOT" :: value :: tail => parse(map ++ Map('plot-> value), tail)
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

  /**
   * IO and graph creating wrapper from results of [[com.kmh.ngs.analses.ReadStatsByIndex]]
   * object.
   *
   * @param results contained in an [[Array[com.kmh.ngs.analyses.ReadStatsByIndex]]]
   * @param output the [[Option[java.io.OutputStreamWriter]]] to write stats file to.
   * @param userOpts the map containing command-line arguments.
   */
  def processResults(results: Array[ReadIndexData], 
  	output: Option[OutputStreamWriter], 
	userOpts: OptionMap): Unit = {
    val ReadStatsHeader = 
   	Array[String]("Index", "N", "Min", "Max", "Sum",
                      "Mean", "StdDev", "Median",
                      "A_ct", "C_ct", "G_ct", "T_ct", "N_ct").mkString("\t")
    output match {
      case Some(output) => {
        if (userOpts.isDefinedAt('plot)) {
          val dataArray = results.toArray.view.zipWithIndex.map{
            case(v, i) => 
              "%s\t%s\t%s\t%s\t%s\t".format(i, v.counts, v.min, v.max, v.sum) +
              "%2.2f\t%2.2f\t%s\t".format(v.mean, v.stdev, v.med) +
              "%s\t%s\t%s\t%s\t%s".format(v.nA, v.nC, v.nG, v.nT, v.nN)}
          output.write(ReadStatsHeader + "\n")
          output.write(dataArray.mkString("\n") + "\n") 
          MultiQualBasesPlot(ReadStatsHeader + "\n" + dataArray.mkString("\n") + "\nend\n",
		userOpts('plot).toString)
	}
        else {    
          output.write(ReadStatsHeader + "\n")
          results.toArray.view.zipWithIndex.foreach {
            case(v, i) =>
              output.write("%s\t%s\t%s\t%s\t%s\t".format(i, v.counts, v.min, v.max, v.sum) +
                           "%2.2f\t%2.2f\t%s\t".format(v.mean, v.stdev, v.med) +
                           "%s\t%s\t%s\t%s\t%s".format(v.nA, v.nC, v.nG, v.nT, v.nN) + "\n")
          }
        }
      }
      case None => {
        if (userOpts.isDefinedAt('plot)) {
          val dataArray = results.view.zipWithIndex.map{
            case(v, i) => 
              "%s\t%s\t%s\t%s\t%s\t".format(i, v.counts, v.min, v.max, v.sum) +
              "%2.2f\t%2.2f\t%s\t".format(v.mean, v.stdev, v.med) +
              "%s\t%s\t%s\t%s\t%s".format(v.nA, v.nC, v.nG, v.nT, v.nN)}.toArray
          println(ReadStatsHeader)
          println(dataArray.mkString("\n"))
          MultiQualBasesPlot(ReadStatsHeader + "\n" + dataArray.mkString("\n") + "\nend\n",
		userOpts('plot).toString)
        }
        else {
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
  }

  /**
   * Creates an iterator for a stats file
   *
   * @param in a [[java.io.BufferedReader]] for the statistics file.
   * @return [[Iterator[String]]] for each line.
   */
  def loadStatFile(in: BufferedReader): Iterator[String] = {
    val it = Iterator.continually {in.readLine()}
    for (line <- it.takeWhile(_ != null)) yield { line }
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

       case (inputBuffer, None, None) =>
         try {
          val dataArray = loadStatFile(inputBuffer).toArray
          // Make sure the file is in the correct format
          if (!dataArray(0).startsWith("Index") || dataArray(0).split("\t").length != 13)
            throw new RuntimeException("Incorrect format! '%s' ".format(userOpts('instat).asInstanceOf[File].getName()))
          // If ok, then plot
          MultiQualBasesPlot(dataArray.mkString("\n") + "\nend\n",
	  	userOpts('plot).toString)
         } catch {
           case err: Throwable => log.error(throw new Exception(err))
         } finally ioInit.closer(inputBuffer)
       // Should never happen
       case _ =>
         log.error(throw new Exception("Something is very wrong")); sys.exit(1)
    }
  } 

}
