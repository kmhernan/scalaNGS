/**
 * Kyle Hernandez
 * kmhernan@utexas.edu
 * 
 * SEQtools
 */

object SEQtools {

  type OptionMap = Map[Symbol, Any]

  def main(args: Array[String]): Unit={
    // Parse arguments 
    if (args.length == 0) mainUsage
    val arglist = args.toList
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
        case option :: tail => println("Unknown option "+option); mainUsage; sys.exit(1)
      }
    }

    val options = nextOption(Map(),arglist)
    
    for {rec <- scala.io.Source.fromFile(options('infile).toString).getLines() grouped 4} {
        println(rec)
    }
  }

  def mainUsage(): Unit={
    println("usage: scala SEQtools -i file.fastq -o file.fastq --start [int] --hpoly [Double] --qual [Int]")
    println("RADtools error: choose a platform")
    sys.exit(1)
  }

}
