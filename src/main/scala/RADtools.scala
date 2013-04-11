/**
 * Kyle Hernandez
 * kmhernan@utexas.edu
 * 
 * RADtools - Various tools for handling RAD NGS sequences.
 */
import com.read._

object RADtools {

  def main(args: Array[String]): Unit={
   
    // Parse arguments 
    if (args.length == 0) Usage
    val arglist = args.toList
    type OptionMap = Map[Symbol, Any]

    def nextOption(map: OptionMap, list: List[String]): OptionMap = {
      list match {
        case Nil => map
        case "-i" :: value :: tail => nextOption(map ++ Map('infile -> value), tail)
        case "-o" :: value :: tail => nextOption(map ++ Map('outfile -> value), tail) 
        case "--start" :: value :: tail => nextOption(map ++ Map('start -> value.toInt), tail)
        case "--end" :: value :: tail => nextOption(map ++ Map('end -> value.toInt), tail)
        case "--hpoly" :: value :: tail => nextOption(map ++ Map('hpoly -> value.toDouble), tail)
        case "--maxN" :: value :: tail => nextOption(map ++ Map('maxN -> value.toInt), tail)
        case "--qual" :: value :: tail => nextOption(map ++ Map('qual -> value.toInt), tail)
        case "--qv-offset" :: value :: tail => nextOption(map ++ Map('qv_offset -> value.toInt), tail)
        case option :: tail => println("Unknown option "+option); Usage; sys.exit(1) 
      }
    }  

    val options = nextOption(Map(),arglist)
    
    for (record <- 
         FastqTools.parseRadSE(options('infile).toString, 
                               options('start).asInstanceOf[Int], 
 			       options('end).asInstanceOf[Int], 
                               options('qv_offset).asInstanceOf[Int])){
      if (record.isNotMissing(10))
        println(record.getBarcode)
    }
  }

  def Usage(): Unit={
    println("-"*80)
    println("Kyle Hernandez - kmhernan@utexas.edu")
    println("RADtools - Various tools for handling RAD NGS sequences.")
    println("-"*80)
    println("""
	USAGE: scala RADtools -i file.fastq -o file.fastq --start [Int] --end [Int]
		              --hpoly [Double] --maxN [Int] --qual [Int] --qv-offset [Int]""")
    sys.exit(1)
  }
}
