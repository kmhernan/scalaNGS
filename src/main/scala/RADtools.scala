/**
 * Kyle Hernandez
 * kmhernan@utexas.edu
 * 
 * RADtools - Various tools for handling RAD NGS sequences.
 */
import com.read._

object RADtools {

  def main(args: Array[String]): Unit={
    
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
        case "--minN" :: value :: tail => nextOption(map ++ Map('minQ -> value.toInt), tail)
        case option :: tail => println("Unknown option "+option); sys.exit(1)
      }
    }  

    val options = nextOption(Map(),arglist)
    
    for (record <- FastqTools.parseRadSE(options('infile).toString, options('start).toInt, options('end).toInt, 33)){
      if (record.isNotMissing(nLimit))
        record.repr
    }
  }

  def Usage(): Unit={
    println("WTF")
  }
}
