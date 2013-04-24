/**
 * Kyle Hernandez
 * kmhernan@utexas.edu
 * 
 * NGSTools - Parent program
 */

package com.kmh.ngs.cmdline
import com.kmh.ngs.tools._

/**
 * Represents the basic attributes of a Command-line Application
 * @return an instance of the tool requested
 */
abstract class CommandLineApp {

  /**
   * The overarching name of the NGSTools application.
   *
   */
  val programName = "NGSTools" 

  /**
   * The pre-usage version, author, and description string.
   *
   */
  val preUsage = "-" * 80 + "\n" + 
                 "NGSTools: Version 1.0.0. UNLICENSED: http://unlicense.org/\n" +
                 "\t2013, Kyle Hernandez. Suite of command-line tools for\n" +
                 "\tNGS reads.\n" +
                 "-" * 80 + "\n"

  /**
   * Minimal main usage statement
   *
   */
  val mainUsage = preUsage +
                  "Select a tool to use...\n" +
                  "Usage: java -jar NGSTools.jar -T [Tool] -h\n"

  /**
   * Verbose main usage statement when the user has -h but hasn't
   * selected a tool.
   *
   */
  val mainVerboseUsage = preUsage +
                         "Usage: java -jar NGSTools.jar -T [Tool] -h\n" +
                         "\t-T\t" + "FilterReads, etc.\n" 

  /**
   * @method getAnalysis
   * @param list - command line arguments in list form
   * @return instance of tool requested
   *
   */
  protected def getAnalysis(list: List[String]) = {
    list match {
      case Nil => println(mainUsage); sys.exit(1)
      case "-h" :: tail => println(mainVerboseUsage); sys.exit(0)
      case "-T" :: value :: tail => loadTool(value, tail)
      case option :: tail => println("Unknown Option "+option); println(mainUsage); sys.exit(1)
     }
  } 

  /**
   * @method loadTool
   * @param value - the name of the user-requested tool in string format
   * @param list - the rest of the user-requested command-line arguments as a list
   * @return instance of the requested tool
   *
   */
  protected def loadTool(value: String, list: List[String]) = {
    value match {
      case "FilterReads" => new FilterReads(list)
      case option => println("Uknown tool"+option); println(mainUsage); sys.exit(1)
    }
  } 

}
