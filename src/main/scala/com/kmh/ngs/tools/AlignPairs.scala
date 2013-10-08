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

import com.kmh.ngs.readers.{ReadReader, PEFastqReader}
import com.kmh.ngs.analyses.GetPairAlignments
import com.kmh.ngs.formats.Read

import java.io.{File, BufferedReader, OutputStreamWriter}
import scala.collection.mutable

import org.eintr.loglady.Logging

/**
 * Align paired-end reads, particularly useful for Juenger Lab 3' seq paired end. 
 * 
 * @constructor args a list of command-line arguments
 */
class AlignPairs(val args: List[String]) extends NGSApp with Logging {

  def toolName = "'%s'".format(this.getClass()) 

  def description = "Align paired-end reads, particularly usefulu for Juenger Lab 3' seq paired ends."

  val SP = " " * ("usage: java -jar NGSTools.jar ".length)
  
  val required = List('offset, 'oOverlap, 'oR1, 'oR2, 'inR1, 'inR2)
 
  def checkRequired(map: OptionMap): OptionMap = {
    if (map.isEmpty) {
      mainUsage
      sys.exit(0)
    }
 
    else if (required.forall(map.isDefinedAt(_))) {
      map
    }

    else {
      mainUsage
      log.error(throw new IllegalArgumentException("Missing required arguments!!"))
      sys.exit(1)
    }
  }

  def mainUsage = List(
    "usage: java -jar NGSTools.jar -T AlignPairs -R1 file_R1.fastq -R2 file_R2.fastq ", 
    SP+"-QV-OFFSET [33,64] -OUT-OVERLAP file_consensus.fastq ", 
    SP+"-OUT-R1 file_R1.fastq -OUT-R2 file_R2.fastq ", 
    SP+"[-h/--help]\n").foreach(println(_))

  def mainVerboseUsage = {
    mainUsage
    List("Required arguments:",
      "  -R1          " + "Input read 1 fastq file",
      "  -R2          " + "Input read 2 fastq file",
      "  -QV-OFFSET   " + "Phred-scaled offset [33, 64]",
      "  -OUT-OVERLAP " + "Output consensus sequence in single-end FASTQ file for overlapping pairs",
      "  -OUT-R1      " + "Output non-overlapping read 1 FASTQ file", 
      "  -OUT-R2      " + "Output non-overlapping read 2 FASTQ file\n").foreach(println(_)) 
    List("Optional Arguments:",
      "  -h/--help    " + "Print this message and exit.\n").foreach(println(_))
  }

  /**
   * Parses command-line options into [[com.kmh.ngs.NGSApp.OptionMap]]
   * 
   * @throws IllegalArgumentException
   */ 
  def parse(map: OptionMap, list: List[String]): OptionMap = {
    list match {
      case Nil => checkRequired(map)
      case "-R1" :: file :: tail => parse(map ++ Map('inR1-> new File(file)), tail)
      case "-R2" :: file :: tail => parse(map ++ Map('inR2-> new File(file)), tail)
      case "-OUT-OVERLAP" :: file :: tail => parse(map ++ Map('oOverlap-> new File(file)), tail)
      case "-OUT-R1" :: file :: tail => parse(map ++ Map('oR1-> new File(file)), tail)
      case "-OUT-R2" :: file :: tail => parse(map ++ Map('oR2-> new File(file)), tail)
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
   */
  def loadReader(userOpts: OptionMap): (List[BufferedReader], List[OutputStreamWriter], PEFastqReader) = {
    val inR1File = userOpts('inR1).asInstanceOf[File]
    val inR2File = userOpts('inR2).asInstanceOf[File]
    ioInit.assertFileIsReadable(inR1File)
    ioInit.assertFileIsReadable(inR2File)
    val inR1Buffer = ioInit.openFileForBufferedReading(inR1File)
    val inR2Buffer = ioInit.openFileForBufferedReading(inR2File)
    val outputR1 = ioInit.openFileForWriting(userOpts('oR1).asInstanceOf[File])
    val outputR2 = ioInit.openFileForWriting(userOpts('oR2).asInstanceOf[File])
    val outputOver = ioInit.openFileForWriting(userOpts('oOverlap).asInstanceOf[File])
    (List(inR1Buffer, inR2Buffer), List(outputR1, outputR2, outputOver), 
     new PEFastqReader(inR1Buffer, Some(inR2Buffer), inR1File, Some(inR2File), None, None, None, None))
  }


  /**
   * The main function for analyzing reads. 
   * 
   * @throws [[IllegalArgumentException]]
   * @throws [[RuntimeException]]
   * @throws [[Exception]]
   */ 
  def run = {
    val userOpts = parse(Map(), args) 
    val (inputBuffer, outputBuffer, readReader) = loadReader(userOpts)
    try {
      GetPairAlignments(outputBuffer, readReader, userOpts('offset).asInstanceOf[Int]) 
    }
    catch {
      case err: Throwable => log.error(throw new Exception(err))
    } 
    finally {
      inputBuffer.map(ioInit.closer(_))
      outputBuffer.map(ioInit.closer(_))
    }
  } 

}
