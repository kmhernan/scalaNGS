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
    val start = System.currentTimeMillis()
    // Parse arguments
    val Parser = com.lib.argparse.RADtoolsOptions 
    if (args.length == 0) Parser.mainUsage
    val options = Parser.parseOpts(args.toList, Map())
    // Get file basename 
    val sample = new java.io.File(options('infile).toString)

    // Filter reads according to user inputs
    val ct_map = {
      options('function) match {
        case "illumina" => com.tools.filters.FilterRAD.illuminaFilter(options)
        case "solid" => com.tools.filters.FilterRAD.illuminaFilter(options)
      }
    }

    // Print summary log
    println("File="+sample.getName()+" Total="+ct_map("Total")
            +" LowQ="+ct_map("LowQ")+" Hpoly="+ct_map("Hpoly")
            +" Passed="+ct_map("Passed")
            +" Ratio="+"%.2f".format(ct_map("Passed")/ct_map("Total").toFloat)
            +" Time="+((System.currentTimeMillis() - start) / 1000.00)+"s")    
  }

}
