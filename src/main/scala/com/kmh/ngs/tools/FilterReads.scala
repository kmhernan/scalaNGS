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
import com.kmh.ngs.formats._
import java.io.File
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
  def mainUsage = List(description, 
    "usage: java -jar NGSTools.jar -T FilterReads [-h/--help] -P/-PLATFORM [solid/SE_illumina/PE_illumina]\n").map(println(_))
  def mainVerboseUsage = {
    mainUsage 
    List("Required Arguments:",
         "  -P/-PLATFORM\tChoose solid (currently only SE reads), " +
         "SE_illumina (single), or PE_illumina (paired) reads.\n").map(println(_))
    List("Optional Arguments:",
	 "  -h/--help\tPrint this message and exit.\n").map(println(_))
  } 
  val supportedPlatforms = List("solid", "SE_illumina", "PE_illumina")

  /**
   * Parses out the platform of the reads 
   *
   * @throws [[IllegalArgumentException]]
   * @return platform of the reads
   * @return list of the remaining args
   */
  private def platform: (String, List[String])={
    this.args match {
      case Nil => log.warn("Please select a platform\n"); mainUsage; sys.exit(1)
      case "-h" :: tail => mainVerboseUsage; sys.exit(0)
      case "--help" :: tail => mainVerboseUsage; sys.exit(0)
      case "-P" :: value :: tail => (value, tail)
      case "-PLATFORM" :: value :: tail => (value, tail)
      case option :: tail =>
        mainUsage; 
	log.error(throw new IllegalArgumentException(
          "Unknown Option; Must specify platform first"));
          sys.exit(1);
    }
  } 

  /**
   * The main function for filtering reads. 
   * 
   * @throws [[IllegalArgumentException]]
   */ 
  def run = {
    val (pltfrm, otherArgs) = platform 
    pltfrm match {
      case "solid" => val userOpts = FilterSolidArgs(otherArgs)
      case option =>  
        mainUsage;
        log.error(throw new IllegalArgumentException("Unknown platform "+pltfrm));
        sys.exit(1) 
    }
    println(userOpts)
  } 

}
