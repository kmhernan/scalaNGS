/**
 * Kyle Hernandez
 * kmhernan@utexas.edu
 * 
 * RADtools - Various tools for handling RAD NGS sequences.
 */
import com.read.seq._

object RADtools {

  type OptionMap = Map[Symbol, Any]

  def main(args: Array[String]): Unit={
    // Parse arguments 
    if (args.length == 0) mainUsage
    val arglist = args.toList
    //type OptionMap = Map[Symbol, Any]
    val options = parseOptions(Map(),arglist)
  }

  def parseOptions(map: OptionMap, list: List[String]): OptionMap = {
    list match {
      case Nil => map
      case "-h" :: tail => verboseMainUsage; sys.exit(1)
      case "illumina" :: tail => nextIlluminaOption(map ++ Map('type -> "illumina"), tail)
      case "solid" :: tail => nextSOLiDOption(map ++ Map('type -> "solid"), tail)
      case option :: tail => println("Unknown option "+option); mainUsage; sys.exit(1)
    }
  }

  def nextSOLiDOption(map: OptionMap, list: List[String]): OptionMap = {
    list match {
      case Nil => map
      case "-i" :: value :: tail => nextSOLiDOption(map ++ Map('infile -> value), tail)
      case "-o" :: value :: tail => nextSOLiDOption(map ++ Map('outfile -> value), tail) 
      case "--start" :: value :: tail => nextSOLiDOption(map ++ Map('start -> value.toInt), tail)
      case "--end" :: value :: tail => nextSOLiDOption(map ++ Map('end -> value.toInt), tail)
      case "--hpoly" :: value :: tail => nextSOLiDOption(map ++ Map('hpoly -> value.toDouble), tail)
      case "--maxN" :: value :: tail => nextSOLiDOption(map ++ Map('maxN -> value.toInt), tail)
      case "--qual" :: value :: tail => nextSOLiDOption(map ++ Map('qual -> value.toInt), tail)
      case "--qv-offset" :: value :: tail => nextSOLiDOption(map ++ Map('qv_offset -> value.toInt), tail)
      case option :: tail => println("Unknown option "+option); solidUsage; sys.exit(1)
      case _ => solidUsage; sys.exit(1) 
    }
  }
  
  def nextIlluminaOption(map: OptionMap, list: List[String]): OptionMap = {
    list match {
      case Nil => map
      case "-h" :: tail => verboseIlluminaUsage; sys.exit(1)
      case "-i" :: value :: tail => nextIlluminaOption(map ++ Map('infile -> value), tail)
      case "-o" :: value :: tail => nextIlluminaOption(map ++ Map('outfile -> value), tail) 
      case "--start" :: value :: tail => nextIlluminaOption(map ++ Map('start -> value.toInt), tail)
      case "--end" :: value :: tail => nextIlluminaOption(map ++ Map('end -> value.toInt), tail)
      case "--hpoly" :: value :: tail => nextIlluminaOption(map ++ Map('hpoly -> value.toDouble), tail)
      case "--maxN" :: value :: tail => nextIlluminaOption(map ++ Map('maxN -> value.toInt), tail)
      case "--qual" :: value :: tail => nextIlluminaOption(map ++ Map('qual -> value.toInt), tail)
      case "--qv-offset" :: value :: tail => nextIlluminaOption(map ++ Map('qv_offset -> value.toInt), tail)
      case option :: tail => println("Unknown option "+option); illuminaUsage; sys.exit(1) 
      case _ => illuminaUsage; sys.exit(1)
    }
  }

  def mainUsage(): Unit={
    println("usage: scala RADtools [-h] {solid,illumina} ...")
    println("RADtools error: choose a platform")
    sys.exit(1)
  }

  def verboseMainUsage(): Unit={
    println("usage: scala RADtools [-h] {solid,illumina} ...\n")
    println("Various tools for Illumina or SOLiD RAD reads - Kyle Hernandez - kmhernan@utexas.edu\n")
    println("optional arguments:")
    println("  -h		show this help message and exit\n")
    println("Platform:\n  Which platform are your reads?\n")
    println("{solid,illumina}")
    println("  solid		Trim and filter SOLiD color-space csfasta files.")
    println("  illumina	Trim and filter Illumina fastq files.")
    sys.exit(1)
  }

  def solidUsage(): Unit={
    println("-"*80)
    println("Kyle Hernandez - kmhernan@utexas.edu")
    println("RADtools - Various tools for handling RAD NGS sequences.")
    println("-"*80)
    println("""USAGE: scala RADtools -i file.fastq -o file.fastq --start [Int] --end [Int]
	              --hpoly [Double] --maxN [Int] --qual [Int] --qv-offset [Int]""")
  }

  def verboseSolidUsage(): Unit={
    println("-"*80)
    println("Kyle Hernandez - kmhernan@utexas.edu")
    println("RADtools - Various tools for handling RAD NGS sequences.")
    println("-"*80)
    println("""USAGE: scala RADtools -i file.fastq -o file.fastq --start [Int] --end [Int]
	              --hpoly [Double] --maxN [Int] --qual [Int] --qv-offset [Int]""")
  }
  def illuminaUsage(): Unit={
    println("-"*80)
    println("Kyle Hernandez - kmhernan@utexas.edu")
    println("RADtools - Various tools for handling RAD NGS sequences.")
    println("-"*80)
    println("""USAGE: scala RADtools -i file.fastq -o file.fastq --start [Int] --end [Int]
	              --hpoly [Double] --maxN [Int] --qual [Int] --qv-offset [Int]""")
  }

  def verboseIlluminaUsage(): Unit={
    println("-"*80)
    println("Kyle Hernandez - kmhernan@utexas.edu")
    println("RADtools - Various tools for handling RAD NGS sequences.")
    println("-"*80)
    println("""USAGE: scala RADtools -i file.fastq -o file.fastq --start [Int] --end [Int]
	              --hpoly [Double] --maxN [Int] --qual [Int] --qv-offset [Int]""")
  }
}
