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
import com.kmh.ngs.cmdline.ReadStatisticsArgs
import com.kmh.ngs.formats.Read
import java.io.{File, BufferedReader, OutputStreamWriter}
import org.eintr.loglady.Logging
import scala.collection.mutable
import com.kmh.ngs.statistics.ReadStatsByIndex

/**
 * Creates summary statistics and figures for NGS sequence data. 
 * 
 */
class ReadStatistics(val args: List[String]) extends NGSApp with Logging {
  def toolName = "'%s'".format(this.getClass()) 
  def description = "Calculates summary statistics for NGS reads."
  val SP = " " * ("usage: java -jar NGSTools.jar ".length)
  val required = List('infq, 'ofil, 'offset)

  def mainUsage = List(
    "usage: java -jar NGSTools.jar -T FilterReads -I/-INPUT file.fastq",
    SP+"-O/-OUTPUT file.fastq -QV-OFFSET {33,64} [-h/--help]\n").map(println(_))

  def mainVerboseUsage = {
    mainUsage
    List("Required Arguments:",
      "  -I/-INPUT\tInput raw read files: <file.fastq> or <file.fastq.gz>",
      "  -O/-OUTPUT\tOutput stats file: <file.txt>",
      "  -QV-OFFSET\tPhred-scaled offset [33, 64]\n").map(println(_))
    List("Optional Arguments:",
      "  -plot\tIf you also want to produce a plot, place file prefix here (no extension)",
      "       \tRequires GNUplot!",
      "  -h/--help\tPrint this message and exit.\n").map(println(_))
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
      case "-O" :: file :: tail => parse(map ++ Map('ofil-> new File(file)), tail)
      case "-OUTPUT" :: file :: tail => parse(map ++ Map('ofil-> new File(file)), tail)
      case "-QV-OFFSET" :: value :: tail => parse(map ++ Map('offset->value.toInt), tail)
      case "-plot" :: value :: tail => parse(map ++ Map('plot-> new File(value+".png"), tail)
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
  def loadReader(userOpts: OptionMap): (BufferedReader, OutputStreamWriter, ReadReader) = 
    val inputFile = userOpts('infq).asInstanceOf[File]
    val outputFile =  
   inputFileList.map(ioInit.openFileForBufferedReading(_))
        val inputBufferList = inputFileList.map(ioInit.openFileForBufferedReading(_))
        //val outputBufferList = outputFileList.map(ioInit.openFileForWriting(_)) 
        (inputBufferList, 
         new FastqReader(inputBufferList(0), inputFileList(0), 
		 if (userOpts.isDefinedAt('start)) Some(userOpts('start).asInstanceOf[Int]) else None,
		 if (userOpts.isDefinedAt('end)) Some(userOpts('end).asInstanceOf[Int]) else None))
      }
    }

  /**
   * The main function for filtering reads. 
   * 
   * @throws [[IllegalArgumentException]]
   */ 
  def run = {
    val userOpts = platform
    val (inputBufferList, readReader) = loadReader(userOpts)
    ReadStatsByIndex(readReader, userOpts('offset).asInstanceOf[Int]) 
    inputBufferList.map(ioInit.closer(_))
    //outputBufferList.map(ioInit.closer(_))
  } 

}
