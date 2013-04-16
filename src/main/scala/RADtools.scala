/**
 * Kyle Hernandez
 * kmhernan@utexas.edu
 * 
 * RADtools - Various tools for handling RAD NGS sequences.
 */

import com.read.seq._
import com.lib.argparse

object RADtools {

  def main(args: Array[String]): Unit={
    // Parse arguments
    val Parser = com.lib.argparse.RADtoolsOptions 
    if (args.length == 0) Parser.mainUsage
    val options = Parser.parseOpts(args.toList, Map()) 
    println(options)
  }

}
