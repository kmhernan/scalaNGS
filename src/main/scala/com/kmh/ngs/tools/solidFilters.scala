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
import com.kmh.ngs.readers._
import scala.collection.mutable
/**
 * Filters SOLiD reads
 *
 */
object solidFilters extends Logging {

  /**
   * Declare usage variables and count map
   *
   */
  private var ct_map = 
    scala.collection.mutable.Map[String, Int](
        "Total Reads"->0,
        "Homopolymer"->0,
        "Low Quality"->0,
        "Missing Base"->0,
        "Passed"->0)
  private val mainUsage = 
    "usage: java -jar NGSTools.jar -T FilterReads -P/-PLATFORM solid -I/-INPUT file.csfasta file.qual\n" +
    "-O/-OUTPUT file.csfasta file.qual [-START Int] [-END Int] [-HPOLY Double] [-MINQ Int]\n"
  private val mainVerboseUsage = mainUsage +
    "REQUIRED:\n" +
    "  -I/-INPUT\tInput raw read files: <file.csfasta> <file.qual>\n" +
    "  -O/-OUTPUT\tOutput filtered read files: <file.csfasta> <file.qual>\n\n" +
    "OPTIONAL (Automatically removed reads with missing bases):\n" +
    "  -START\t5' cut position (1-based index)\n" +
    "  -END\t3' cut position (1-based index)\n" +
    "      \tex. AlfI: -START 1 -END 36\n" +
    "  -HPOLY\tRelative length of repetitive base to consider a homopolymer.\n" +
    "        \t(Proportion of read length; e.g., between 0 and 1)\n" +
    "  -MINQ\tMinimum average quality score allowed.\n"
  private val required = List("incsfa", "incsq", "ocsfa", "ocsq")
 
   /**
   * @method parseSolid - Parses SOLiD arguments
   * @param OptionMap
   * @param a list of the arguments
   * @return OptionMap
   */
  private def parseSolid(map: OptionMap, list: List[String]): OptionMap = {
    list match {
      case Nil => checkRequired(map)
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

  def checkRequired(map: OptionMap): OptionMap = {
    if required.forall(x => map.isDefinedAt(x)) 
      map
    else { 
      log.warn(mainUsage)
      log.error(throw new Exception("Two input and output files required for SOLiD reads!!"))
      sys.exit(1)
    }
  }
 
  def main(args: List[String]): Unit = {
    
    log.info("TOTAL="+ct_map("Total Reads")+" "+
             "MISSING="+ct_map("Missing Base")+" "+
             "LOWQ="+ct_map("Low Quality")+" "+
             "HOMOPOLYMER="+ct_map("Homopolymer")+" "+
             "PASSED="+ct_map("Passed"))
  }

}  