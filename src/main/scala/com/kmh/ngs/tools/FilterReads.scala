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
import com.kmh.ngs.filters._
import java.io.File
import org.eintr.loglady.Logging
import scala.collection.mutable

/**
 * @class FilterReads - Filters NGS reads based on user inputs 
 * @param args - list of command-line arguments
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
    "usage: java -jar NGSTools.jar -T FilterReads [-h/--help] -P/-PLATFORM [solid/illumina]\n" +
    "Arguments:\n" + "\t-h/--help\tPrint this message.\n" + 
    "\t-P/-PLATFORM\tChoose solid or illumina reads.\n"

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
      case option :: tail => 
        log.warn(mainUsage);
	log.error(throw new IllegalArgumentException(
          "Unknown Option; Must specify platform first"));
          sys.exit(1);
    }
  } 

  /**
   * @method run - the main function for filtering reads 
   *
   */ 
  def run = {
    val (pltfrm, otherArgs) = this.platform 
    pltfrm match {
      case "solid" => solidFilters.main(otherArgs)
      case "illumina" => illuminaFilters.main(otherArgs)
      case option => 
        log.warn(mainUsage);
    	log.error(throw new IllegalArgumentException("Unknown platform "+option));
	sys.exit(1); 
    }
  } 

}
