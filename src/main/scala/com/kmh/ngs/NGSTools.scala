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

package com.kmh.ngs
import com.kmh.ngs.tools._
import org.eintr.loglady.Logging

/**
 * @author Kyle Hernandez
 * @object NGSTools - Command line interface for running various
 *                    ngs tools
 *
 */
object NGSTools extends Logging {
  /**
   * Initialize the logger and declare usage
   * statements.
   *
   */
  val programName = "NGSTools"
  val header      = "NGSTools v1.0.0 - 2013, Kyle Hernandez, UNLICENSED: http://unlicense.org/" 
  val desc        = "A suite of tools for NGS data written in the Scala language"
  val preUsage    = "NGSTools: Version 1.0.0. UNLICENSED: http://unlicense.org/\n" +
                    "\t2013, Kyle Hernandez. Suite of command-line tools for\n" +
                    "\tNGS reads.\n"
  val mainUsage   = preUsage +
                    "Select a tool to use...\n" +
                    "Usage: java -jar NGSTools.jar -T [Tool] -h\n"
  val mainVerboseUsage = preUsage +
                         "Usage: java -jar NGSTools.jar -T [Tool] -h\n" +
                         "\t-T\t" + "FilterReads, etc.\n"

  /**
   * @function getAnalysis
   * @param list - command line arguments in list form
   * @return instance of tool requested
   *
   */
  protected def getAnalysis(list: List[String])={
    list match {
      case Nil => log.warn(mainUsage); sys.exit(1)
      case "-h" :: tail => log.info(mainVerboseUsage); sys.exit(0)
      case "-help" :: tail => log.info(mainVerboseUsage); sys.exit(0)
      case "-T" :: value :: tail => loadTool(value, tail)
      case "-TOOL" :: value :: tail => loadTool(value, tail)
      case option :: tail => log.warn(mainUsage);
		  	     log.error(throw new IllegalArgumentException("Unknown Option "+option));
		             sys.exit(1);
     }
  }

   /**
   * @function loadTool
   * @param value - the name of the user-requested tool in string format
   * @param list - the rest of the user-requested command-line arguments as a list
   * @return instance of the requested tool
   *
   */
  protected def loadTool(value: String, list: List[String]) = {
    value match {
      case "FilterReads" => new FilterReads(list)
      case option => log.info(mainUsage);
		     log.error(throw new IllegalArgumentException("Unknown Option "+option));
                     sys.exit(1);
    }
  }

  /**
   * @function main - This is the main wrapper function for all applications.
   * @param args - Command-line arguments
   *
   */ 
  def main(args: Array[String]): Unit = {
    log.info("-"*80)
    log.info(header)
    log.info(desc)
    log.info("-"*80)

    val start = System.currentTimeMillis()
    val analysis = getAnalysis(args.toList)
    log.info("Arguments: " + args.mkString(" "))
    log.info("Starting '%s'".format(analysis.getClass))

    analysis.run

    log.info("Finished: '%s' took '%.3f' seconds".format(
     	analysis.getClass, (System.currentTimeMillis() - start) / 1000.00))
  }

}
