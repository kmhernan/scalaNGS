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

//import com.kmh.ngs.readers.{ReadReader, FastqReader}
//import com.kmh.ngs.formats.Read
//import com.kmh.ngs.analyses.{ReadStatsByIndex, ReadIndexData}
//import com.kmh.ngs.plotting.MultiQualBasesPlot

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
class SAMStatistics(val args: List[String]) extends NGSApp with Logging {
  def toolName = "'%s'".format(this.getClass()) 
  def description = "Calculates summary statistics for SAM files "
  val SP = " " * ("usage: java -jar NGSTools.jar ".length)

  def checkRequired(map: OptionMap): OptionMap = {
    if (map.isEmpty) {
      mainUsage
      sys.exit(0)
    } 
    if (map.isDefinedAt('insam))
      map
    else {
      mainUsage
      log.error(throw new IllegalArgumentException("Must provide input SAM file!!"))
      sys.exit(1)
    }
  }

  def mainUsage = List(
    "usage: java -jar NGSTools.jar -T SAMStatistics -I/-INPUT file.sam [-h/--help]\n").map(println(_))

  def mainVerboseUsage = {
    mainUsage
    List("Required arguments:",
      "-I/-INPUT \tREQUIRED: Input sam file: <file.sam> or <file.sam.gz>").map(println(_))
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
      case "-I" :: file :: tail => parse(map ++ Map('insam-> new File(file)), tail)
      case "-INPUT" :: file :: tail => parse(map ++ Map('insam-> new File(file)), tail)
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
  def loadReader(userOpts: OptionMap): (BufferedReader, SAMReader) = {
    val inputFile    = userOpts('infq).asInstanceOf[File]
    ioInit.assertFileIsReadable(inputFile)
    val inputBuffer  = ioInit.openFileForBufferedReading(inputFile)
    (inputBuffer, new SAMReader(inputBuffer, inputFile)) 
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
  } 

}
