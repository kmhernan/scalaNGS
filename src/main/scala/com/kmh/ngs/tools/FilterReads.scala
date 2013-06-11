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
import com.kmh.ngs.readers.{ReadReader, CSFastaReader, FastqReader, PEFastqReader}
import com.kmh.ngs.cmdline.{FilterSolidArgs, FilterSEIlluminaArgs, FilterPEIlluminaArgs}
import com.kmh.ngs.filters.SequenceFilters
import com.kmh.ngs.formats.Read
import java.io.{File, BufferedReader, OutputStreamWriter}
import org.eintr.loglady.Logging
import scala.collection.mutable

/**
 * Filters NGS reads based on user inputs 
 * 
 * @constructor create FilterReads [[com.kmh.ngs.tools.NGSApp]] with user-given command-line arguments
 * @param args the list of commands-line arguments
 */
class FilterReads(val args: List[String]) extends NGSApp with Logging {
  def toolName = "'%s'".format(this.getClass()) 
  def description = "Filters NGS reads based on user inputs."
  def mainUsage = List(
    "usage: java -jar NGSTools.jar -T FilterReads [-h/--help] " + 
    "-P/-PLATFORM {solid/SE_illumina/PE_illumina}\n").foreach(println(_))
  def mainVerboseUsage = {
    mainUsage 
    List(
    "Required Arguments:",
    "  -P/-PLATFORM\tChoose solid (currently only SE reads), " +
    "SE_illumina (single), or PE_illumina (paired) reads.\n").foreach(println(_))
    List(
    "Optional Arguments:",
    "  -h/--help\tPrint this message and exit.\n").foreach(println(_))
  } 
  val supportedPlatforms = List("solid", "SE_illumina", "PE_illumina")

  /**
   * Parses out the platform of the reads 
   *
   * @throws [[IllegalArgumentException]]
   * @return platform of the reads
   * @return list of the remaining args
   */
  private def platform: OptionMap = {
    args match {
      case Nil => log.warn("Please select a platform\n"); mainUsage; sys.exit(1)
      case "-h" :: tail => mainVerboseUsage; sys.exit(0)
      case "--help" :: tail => mainVerboseUsage; sys.exit(0)
      case "-P" :: value :: tail =>
        value match {
          case "solid" => FilterSolidArgs(tail) ++ Map('platform -> value)
          case "SE_illumina" => FilterSEIlluminaArgs(tail) ++ Map('platform -> value)
          case "PE_illumina" => FilterPEIlluminaArgs(tail) ++ Map('platform -> value)
          case option =>
            mainUsage;
            log.error(throw new IllegalArgumentException("Unknown platform "+option));
            sys.exit(1)
        } 
      case "-PLATFORM" :: value :: tail => 
        value match {
          case "solid" => FilterSolidArgs(tail) ++ Map('platform -> value)
          case "SE_illumina" => FilterSEIlluminaArgs(tail) ++ Map('platform -> value)
          case "PE_illumina" => FilterPEIlluminaArgs(tail) ++ Map('platform -> value)
          case option =>
            mainUsage;
            log.error(throw new IllegalArgumentException("Unknown platform "+option));
            sys.exit(1)
        } 
      case option =>
        mainUsage; 
	log.error(throw new IllegalArgumentException(
          "Unknown Option; Must specify platform first"));
          sys.exit(1);
    }
  } 

  /**
   * Creates lists containing input and output streams and the instance of the read iterator.
   *
   * @param userOpts the map of the command-line arguments
   * @return List[[java.io.BufferedReader] the list of input streams
   * @return List[[java.io.OutputStreamWriter]] the list of output streams
   * @return [[com.kmh.ngs.readers.ReadReader]] an instance of a reader for NGS sequence files.
   */
  def loadReader(userOpts: OptionMap): (List[BufferedReader], List[OutputStreamWriter], ReadReader) = 
    userOpts('platform) match {
      case "solid" => {
        val inputFileList = List(userOpts('incsfa).asInstanceOf[File], userOpts('incsq).asInstanceOf[File])
        val outputFileList = List(userOpts('ocsfa).asInstanceOf[File], userOpts('ocsq).asInstanceOf[File])
        inputFileList.map(ioInit.assertFileIsReadable(_))
        val inputBufferList = inputFileList.map(ioInit.openFileForBufferedReading(_))
        val outputBufferList = outputFileList.map(ioInit.openFileForWriting(_)) 
        (inputBufferList, 
         outputBufferList,
         new CSFastaReader(inputBufferList(0), inputBufferList(1), inputFileList(0), inputFileList(1),
		  if (userOpts.isDefinedAt('start)) Some(userOpts('start).asInstanceOf[Int]) else None,
		  if (userOpts.isDefinedAt('end)) Some(userOpts('end).asInstanceOf[Int]) else None))
      }
      case "SE_illumina" => {
        val inputFileList = List(userOpts('infq).asInstanceOf[File])
        val outputFileList = List(userOpts('outfq).asInstanceOf[File])
        inputFileList.map(ioInit.openFileForBufferedReading(_))
        val inputBufferList = inputFileList.map(ioInit.openFileForBufferedReading(_))
        val outputBufferList = outputFileList.map(ioInit.openFileForWriting(_)) 
        (inputBufferList, 
         outputBufferList,
         new FastqReader(inputBufferList(0), inputFileList(0), 
		 if (userOpts.isDefinedAt('start)) Some(userOpts('start).asInstanceOf[Int]) else None,
		 if (userOpts.isDefinedAt('end)) Some(userOpts('end).asInstanceOf[Int]) else None))
      }
      case "PE_illumina" => {
        if (userOpts.isDefinedAt('inInter)) {
          val inputFileList = List(userOpts('inInter).asInstanceOf[File])
          val outputFileList = 
            if (userOpts.isDefinedAt('outInter)) List(userOpts('outInter).asInstanceOf[File])
            else List(userOpts('outR1), userOpts('outR2)).map(_.asInstanceOf[File])
          inputFileList.map(ioInit.openFileForBufferedReading(_))
          val inputBufferList = inputFileList.map(ioInit.openFileForBufferedReading(_))
          val outputBufferList = outputFileList.map(ioInit.openFileForWriting(_)) 
          (inputBufferList, 
           outputBufferList,
           new PEFastqReader(inputBufferList(0), None, 
                 inputFileList(0), None,
		 if (userOpts.isDefinedAt('start)) Some(userOpts('start).asInstanceOf[Int]) else None,
		 if (userOpts.isDefinedAt('end)) Some(userOpts('end).asInstanceOf[Int]) else None))
        } else {
          val inputFileList = List(userOpts('inR1), userOpts('inR2)).map(_.asInstanceOf[File])
          val outputFileList = 
            if (userOpts.isDefinedAt('outInter)) List(userOpts('outInter).asInstanceOf[File])
            else List(userOpts('outR1), userOpts('outR2)).map(_.asInstanceOf[File])
          inputFileList.map(ioInit.openFileForBufferedReading(_))
          val inputBufferList = inputFileList.map(ioInit.openFileForBufferedReading(_))
          val outputBufferList = outputFileList.map(ioInit.openFileForWriting(_)) 
          (inputBufferList, 
           outputBufferList,
           new PEFastqReader(inputBufferList(0), Some(inputBufferList(1)), 
                 inputFileList(0), Some(inputFileList(1)), 
		 if (userOpts.isDefinedAt('start)) Some(userOpts('start).asInstanceOf[Int]) else None,
		 if (userOpts.isDefinedAt('end)) Some(userOpts('end).asInstanceOf[Int]) else None))
        }
    }
  }

  /**
   * The main function for filtering reads. 
   * 
   * @throws [[IllegalArgumentException]]
   */ 
  def run = {
    val userOpts = platform
    val (inputBufferList, outputBufferList, readReader) = loadReader(userOpts)
    val ct_map = SequenceFilters(readReader, userOpts, outputBufferList)
    inputBufferList.map(ioInit.closer(_))
    outputBufferList.map(ioInit.closer(_))
    readReader match {
      case cs: CSFastaReader =>
        val logCS = new StringBuilder()
        log.info(
             "FILE=[%s, %s] ".format(userOpts('incsfa).asInstanceOf[File].getName(), 
                                     userOpts('incsq).asInstanceOf[File].getName())+
             "TOTAL="+ct_map("Total Reads")+" "+
             "MISSING="+ct_map("Missing Base")+" "+
             "LOWQ="+ct_map("Low Quality")+" "+
             "HOMOPOLYMER="+ct_map("Homopolymer")+" "+
             "PASSED="+ct_map("Passed")+" "+
             "RATIO=%.2f".format(ct_map("Passed")/ct_map("Total Reads").toDouble))
      case fq: FastqReader =>
        log.info(
             "FILE="+userOpts('infq).asInstanceOf[File].getName()+" "+
             "TOTAL="+ct_map("Total Reads")+" "+
             "MISSING="+ct_map("Missing Base")+" "+
             "LOWQ="+ct_map("Low Quality")+" "+
             "HOMOPOLYMER="+ct_map("Homopolymer")+" "+
             "POLYA="+ct_map("Poly A")+" "+
             "TOOSHORT="+ct_map("Too Short")+" "+
             "PASSED="+ct_map("Passed")+" "+
             "RATIO=%.2f".format(ct_map("Passed")/ct_map("Total Reads").toDouble))
      case pefq: PEFastqReader =>
        if (userOpts.isDefinedAt('inInter))
          log.info(
               "FILE="+userOpts('inInter).asInstanceOf[File].getName()+" "+
               "TOTAL="+ct_map("Total Reads")+" "+
               "MISSING="+ct_map("Missing Base")+" "+
               "LOWQ="+ct_map("Low Quality")+" "+
               "HOMOPOLYMER="+ct_map("Homopolymer")+" "+
               "POLYA="+ct_map("Poly A")+" "+
               "TOOSHORT="+ct_map("Too Short")+" "+
               "PASSED="+ct_map("Passed")+" "+
               "RATIO=%.2f".format(ct_map("Passed")/ct_map("Total Reads").toDouble))
        else
          log.info(
               "FILE=[%s, %s] ".format(userOpts('inR1).asInstanceOf[File].getName(), 
                                       userOpts('inR2).asInstanceOf[File].getName())+
               "TOTAL="+ct_map("Total Reads")+" "+
               "MISSING="+ct_map("Missing Base")+" "+
               "LOWQ="+ct_map("Low Quality")+" "+
               "HOMOPOLYMER="+ct_map("Homopolymer")+" "+
               "POLYA="+ct_map("Poly A")+" "+
               "TOOSHORT="+ct_map("Too Short")+" "+
               "PASSED="+ct_map("Passed")+" "+
               "RATIO=%.2f".format(ct_map("Passed")/ct_map("Total Reads").toDouble))
    }
  } 

}
