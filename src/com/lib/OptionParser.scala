/**
 * Kyle Hernandez
 * OptionParser - package to parse options for RADtools and SEQtools
 *
 */

package com.lib.argparse

object RADtoolsOptions {

  type OptionMap = Map[Symbol, Any]

  def parseOpts(args: List[String], map: OptionMap): OptionMap={
    args.head match {
      case "-h" => verboseMainUsage; sys.exit(1) 
      case "illumina" => nextIlluminaOption(map ++ Map('function -> "illumina"), args.tail) 
      case "solid" => nextSOLiDOption(map ++ Map('function -> "solid"), args.tail)
      case option => println("Unknown option "+option); mainUsage; sys.exit(1) 
    } 
  }

  def nextIlluminaOption(map: OptionMap, list: List[String]): OptionMap = {
    list match {
      case Nil  => if (checkRequired(map)) { map } else sys.exit(1)
      case "-h" :: tail => verboseIlluminaUsage(); sys.exit(1)
      case "-i" :: value :: tail => nextIlluminaOption(map ++ Map('infile -> value), tail)
      case "-o" :: value :: tail => nextIlluminaOption(map ++ Map('outfile -> value), tail)
      case "--start" :: value :: tail => nextIlluminaOption(map ++ Map('start -> value.toInt), tail)
      case "--end" :: value :: tail => nextIlluminaOption(map ++ Map('end -> value.toInt), tail)
      case "--hpoly" :: value :: tail => nextIlluminaOption(map ++ Map('hpoly -> value.toDouble), tail)
      case "--qual" :: value :: tail => nextIlluminaOption(map ++ Map('qual -> value.toInt), tail)
      case "--qv-offset" :: value :: tail => nextIlluminaOption(map ++ Map('qv_offset -> value.toInt), tail)
      case option :: tail => println("Unknown option "+option); illuminaUsage; sys.exit(1)
    }
  }

  def nextSOLiDOption(map: OptionMap, list: List[String]): OptionMap = {
    list match {
      case Nil  => if (checkRequired(map)) { map } else sys.exit(1)
      case "-h" :: tail => verboseSolidUsage(); sys.exit(1)
      case "-i" :: value :: tail => nextSOLiDOption(map ++ Map('infile -> value), tail)
      case "-o" :: value :: tail => nextSOLiDOption(map ++ Map('outfile -> value), tail)
      case "--start" :: value :: tail => nextSOLiDOption(map ++ Map('start -> value.toInt), tail)
      case "--end" :: value :: tail => nextSOLiDOption(map ++ Map('end -> value.toInt), tail)
      case "--hpoly" :: value :: tail => nextSOLiDOption(map ++ Map('hpoly -> value.toDouble), tail)
      case "--maxN" :: value :: tail => nextSOLiDOption(map ++ Map('maxN -> value.toInt), tail)
      case "--qual" :: value :: tail => nextSOLiDOption(map ++ Map('qual -> value.toInt), tail)
      case option :: tail => println("Unknown option "+option); solidUsage; sys.exit(1)
    }
  }

  def checkRequired(map: OptionMap): Boolean = {
    if (map.isDefinedAt('infile) && map.isDefinedAt('outfile)) {
      if (map.isDefinedAt('start) && map.isDefinedAt('end)) {
        if(map('function) == "illumina" && map.isDefinedAt('qv_offset))
          true
        else if (map('function) == "illumina" && ! map.isDefinedAt('qv_offset)) {
            println("OptionParser Error: Must specify --qv-offset")
            illuminaUsage
            sys.exit(1)
        } else
             true
      } else {
          println("OptionParser Error: Must specific start/stop positions for trimming")
          if (map('function) == "illumina") {
            illuminaUsage
            sys.exit(1)
          } else {
            solidUsage
            sys.exit(1)
          }
       }
    } else {
        println("OptionParser Error: Must specify input/output files")
        if(map('function) == "illumina") {
          illuminaUsage
          sys.exit(1)
        } else {
          solidUsage
          sys.exit(1) 
      }
    }
  }

  def illuminaUsage(): Unit={
    println("""USAGE: scala RADtools -i file.fastq -o file.fastq --start [Int] --end [Int]
                      --hpoly [Double] --maxN [Int] --qual [Int] --qv-offset [Int]""")
  }

  def verboseIlluminaUsage(): Unit={
    println("""USAGE: scala RADtools -i file.fastq -o file.fastq --start [Int] --end [Int]
                      --hpoly [Double] --maxN [Int] --qual [Int] --qv-offset [Int]""")
  }

  def solidUsage(): Unit={
    println("USAGE: scala RADtools solid [-h] -i file.csfasta file.qual -o file.fastq file.qual " +
            "--start [Int] --end [Int] --hpoly [Double] --qual [Int]")
  }

  def verboseSolidUsage(): Unit={
    println("USAGE: scala RADtools solid [-h] -i file.csfasta file.qual -o file.fastq file.qual " +
            "--start [Int] --end [Int] --hpoly [Double] --qual [Int]\n")
    println("Arguments:")
    println("  -h		show this help message and exit\n")
    println("  -i		Input: File.csfasta File.qual")
    println("  -o		Output: File.csfasta File.qual")
    println("  --start	5' trim position (1-based)")
    println("  --end 	3' trim position (1-based)")
    println("		e.g. AlfI --start 1 --end 36")
    println("  --hpoly	Max length of homopolymer region allowed")
    println("		Expressed as a proportion of the length of the read")
    println("  --qual	Min average quality allowed")	
  }
 
  def mainUsage: Unit={
    println("usage: scala RADtools [-h] {solid,illumina} ...")
    println("RADtools error: choose a platform")
    sys.exit(1)
  }

  def verboseMainUsage: Unit={
    println("usage: scala RADtools [-h] {solid,illumina} ...\n")
    println("Various tools for Illumina or SOLiD RAD reads - Kyle Hernandez - kmhernan@utexas.edu\n")
    println("optional arguments:")
    println("  -h               show this help message and exit\n")
    println("Platform:\n  Which platform are your reads?\n")
    println("{solid,illumina}")
    println("  solid            Trim and filter SOLiD color-space csfasta files.")
    println("  illumina Trim and filter Illumina fastq files.")
    sys.exit(1)
  }

}
