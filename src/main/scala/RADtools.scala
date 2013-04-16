/**
 * Kyle Hernandez
 * kmhernan@utexas.edu
 * 
 * RADtools - Various tools for handling RAD NGS sequences.
 */

import com.read._
import com.lib._
import com.tools._

object RADtools {

  def main(args: Array[String]): Unit={
    // Parse arguments
    val Parser = com.lib.argparse.RADtoolsOptions 
    if (args.length == 0) Parser.mainUsage
    val options = Parser.parseOpts(args.toList, Map()) 
    
    if (options('function) == "illumina")
      com.tools.filters.FilterRAD.illuminaFilter(options) 
  }

}
