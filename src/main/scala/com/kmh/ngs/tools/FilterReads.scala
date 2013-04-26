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
import scala.collection.mutable
import java.io.File
import com.kmh.ngs.csfasta._
import org.eintr.loglady.Logging

/**
 * @class FilterReads - Filters NGS reads based on user inputs 
 *
 */
class FilterReads(val args: List[String]) extends NGSApp with Logging {
  /**
   * Inititalize the variables associated with this 
   * tool. 
   */
  def toolName = "'%s'".format(this.getClass()) 
  def description = "Filters NGS reads based on user inputs."
  def mainUsage = 
    "usage: java -jar NGSTools.jar -T FilterReads [-h/--help] -P/-PLATFORM [solid/illumina]"
  def mainVerboseUsage = 
    "usage: java -jar NGSTools.jar -T FilterReads [-h/--help] -P/-PLATFORM [solid/illumina]"
  var ct_map = scala.collection.mutable.Map[String, Int](
  	"Total Reads"->0,
        "Homopolymer"->0,
        "Low Quality"->0,
        "Missing Base"->0,
        "Passed"->0)

  /**
   * @method parse - Parses the command line arguments and
   * send them to their respective methods
   *
   */
  private def parse = {
    val (userPlatform, otherArgs) = platform
    userPlatform match {
      case "solid" => parseSolid(Map() ++ Map("platform"-> userPlatform), otherArgs)
      //case "illumina" => parseIllumina(Map() ++ Map("platform" -> platform), otherArgs)
      case option => log.warn("Error!! Unknown Platform; Must be 'illumina' or 'solid'"
         		     +option + "\n" + mainUsage); sys.exit(1)
    }
  }  

  /**
   * @method parseSolid - Parses SOLiD arguments
   * @param OptionMap
   * @param a list of the arguments
   * @return OptionMap
   */
  private def parseSolid(map: OptionMap, list: List[String]): OptionMap = {
    list match {
      case Nil => map
      case "-I" :: file1 :: file2 :: tail =>
        if (file2.startsWith("-")) {
          log.warn(
            "Error!! SOLiD reads: -I sequences.csfasta quality.fasta\n" +
            "\tYou need two input files <sequences.csfasta> <quality.fasta>\n" +
          mainUsage)
          sys.exit(1)
        }
        else
          parseSolid(
            map ++ Map("incsfa"-> new File(file1)) ++ Map("incsq" -> new File(file2)), 
            tail)
      case "-O" :: file1 :: file2 :: tail =>
        if (file2.startsWith("-")){
          log.warn(mainUsage)
          log.error(throw new IllegalArgumentException("Error!! SOLiD reads require 2 output files"));
          sys.exit(1);
        }
        else
          parseSolid(
            map ++ Map("ocsfa"-> new File(file1)) ++ Map("ocsq" -> new File(file2)), 
            tail)
      case "-START" :: value :: tail => parseSolid(map ++ Map("start"->value.toInt), tail)
      case "-END" :: value :: tail => parseSolid(map ++ Map("end"->value.toInt), tail)
      case "-HPOLY" :: value :: tail => parseSolid(map ++ Map("hpoly"->value.toDouble), tail)
      case "-MINQ" :: value :: tail => parseSolid(map ++ Map("minq"->value.toInt), tail)
      case option => log.warn(mainUsage);
 		     log.error(throw new IllegalArgumentException("Unknown Option "+option));
		     sys.exit(1);
    }
  }

  /**
   * @method platform - parses out the platform of the reads 
   * @return platform of the reads
   * @return list of the remaining args
   *
   */
  private def platform : (String, List[String])={
    this.args match {
      case Nil => log.warn("Please select a platform\n" + mainUsage); sys.exit(1)
      case "-h" :: tail => log.info(mainVerboseUsage); sys.exit(0)
      case "-P" :: value :: tail => (value, tail)
      case option :: tail => log.warn(mainUsage);
			     log.error(
 			       throw new IllegalArgumentException("Unknown Option; Must specify platform first"));
                             sys.exit(1);
    }
  } 

  //private def parseIllumina(list: List[String]):
 
  /**
   * @method run - the main function for filtering reads 
   *
   */ 
  def run = {
    val userOpts = this.parse
    anyToString(userOpts("platform")) match {
      case "solid" => processSolid(userOpts)
      //case "illumina" => processIllumina(userOpts)
      case option => log.warn(mainUsage);
    		     log.error(throw new IllegalArgumentException("Unknown platform: [solid/illumina]"));
		     sys.exit(1); 
    }
    println(ct_map)
  } 

  private def processSolid(userOpts: OptionMap) = {
    log.info("Processing SOLiD reads...")
    // Initialize IO
    val infa  = anyToFile(userOpts("incsfa"))
    val inq   = anyToFile(userOpts("incsq"))
    val outfa = anyToFile(userOpts("ocsfa"))
    val outq  = anyToFile(userOpts("ocsq"))
    List(infa, inq).map(ioInstance.assertFileIsReadable(_))
    val seqReader  = ioInstance.openFileForBufferedReading(infa)
    val qualReader = ioInstance.openFileForBufferedReading(inq)
    val solidIter  = CSFastaReader.parseCSFasta(
	seqReader, qualReader, infa, inq, 
	if (userOpts.isDefinedAt("start")) Some(anyToInt(userOpts("start"))) else None, 
	if (userOpts.isDefinedAt("end")) Some(anyToInt(userOpts("end"))) else None)
    val seqWriter = ioInstance.openFileForWriting(outfa)
    val qualWriter = ioInstance.openFileForWriting(outq)
    // Run filters
    try {
      for (rec <- solidIter){
        ct_map("Total Reads") += 1
        // check for N's
        if (rec.seqLine.contains("."))
          ct_map("Missing Base") += 1
        else {
          if (rec.averageQuality < anyToInt(userOpts("minq"))) 
            ct_map("Low Quality") += 1
          else {
            if (rec.isHomopolymer(anyToDbl(userOpts("hpoly"))))
              ct_map("Homopolymer") += 1
            else {
              ct_map("Passed") += 1
              rec.writeToFile(seqWriter, qualWriter)
            } 
          } 
        } 
      } 
    } finally {
    	List(seqWriter, qualWriter, seqReader, seqWriter).map(ioInstance.closer(_)) 
    }
    List(seqWriter, qualWriter, seqReader, seqWriter).map(ioInstance.closer(_)) 
  }


}

