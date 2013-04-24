package com.kmh.ngs.tools
import scala.collection.mutable
import java.io.File
import com.kmh.ngs.io._
import com.kmh.ngs.csfasta._

/**
 * Class to filter NGS reads
 *
 */
class FilterReads(args: List[String]) {
  /**
   * Inititalize the variables associated with this 
   * tool. 
   */ 
  type OptionMap = Map[String, Any]
  val ioInstance = new IoUtil
  var ct_map = scala.collection.mutable.Map[String, Int](
  	"Total Reads"->0,
        "Homopolymer"->0,
        "Low Quality"->0,
        "Missing Base"->0,
        "Passed"->0)
  private val preUsage = "-" * 80 + "\n" +
                         "NGTools -T FilterReads. UNLICENSED: httP;//unlicense.org/\n" +
                         "\t2013, Kyle Hernandez. Filter raw NGS reads.\n" +
                         "-" * 80 + "\n"
  private val mainUsage = preUsage +
                          "Usage: java -jar NGSTools.jar -T FilterReads -P [illumina,solid] -h\n"
  private val mainVerboseUsage = preUsage +
                          "Usage: java -jar NGSTools.jar -T FilterReads -P [illumina,solid] -h\n"

  /**
   * platform - parses out the platform of the reads 
   * @return platform of the reads
   * @return list of the remaining args
   *
   */
  private def platform : (String, List[String])={
    this.args match {
      case Nil => println("Please select a platform\n" + mainUsage); sys.exit(1)
      case "-h" :: tail => println(mainVerboseUsage); sys.exit(0)
      case "-P" :: value :: tail => (value, tail)
      case option :: tail => println("Unknown Option; Must specify platform first "
                                     +option+"\n"+mainUsage); sys.exit(1)
    }
  } 

  /**
   * Parses the command line arguments and
   * send them to their respective methods
   *
   */
  private def parse = {
    val (userPlatform, otherArgs) = platform
    userPlatform match {
      case "solid" => parseSolid(Map() ++ Map("platform"-> userPlatform), otherArgs)
      //case "illumina" => parseIllumina(Map() ++ Map("platform" -> platform), otherArgs)
      case option => println("Unknown Platform; Must be 'illumina' or 'solid'"
         		     +option + "\n" + mainUsage); sys.exit(1)
    }
  }  

  /**
   * parseSolid - Parses SOLiD arguments
   * @param OptionMap
   * @param a list of the arguments
   * @return OptionMap
   */
  private def parseSolid(map: OptionMap, list: List[String]): OptionMap = {
    list match {
      case Nil => map
      case "-I" :: file1 :: file2 :: tail =>
           if (file2.startsWith("-")){
             println("Error!! SOLiD reads: -I sequences.csfasta quality.fasta\n" +
                     "\tYou need two input files <sequences.csfasta> <quality.fasta>\n"+mainUsage)
             sys.exit(1)
           }
           else
             parseSolid(map ++ Map("incsfa"-> new File(file1))
                            ++ Map("incsq" -> new File(file2)), 
                            tail)
      case "-O" :: file1 :: file2 :: tail =>
           if (file2.startsWith("-")){
             println("SOLiD reads: -O sequences.csfasta quality.fasta\n" +
                     "\tYou need two files...\n"+mainUsage)
             sys.exit(1)
           }
           else
             parseSolid(map ++ Map("ofasta"-> new File(file1))
                            ++ Map("oqual" -> new File(file2)), 
                            tail)
      case "-START" :: value :: tail => parseSolid(map ++ Map("start"->value.toInt), tail)
      case "-END" :: value :: tail => parseSolid(map ++ Map("end"->value.toInt), tail)
      case "-HPOLY" :: value :: tail => parseSolid(map ++ Map("hpoly"->value.toDouble), tail)
      case "-MINQ" :: value :: tail => parseSolid(map ++ Map("minq"->value.toInt), tail)
      case option => println("Unknown option: "+option+"\n"+mainUsage); sys.exit(1)
    }
  }

  //private def parseIllumina(list: List[String]):
 
  /**
   * Functions to run the tool
   *
   */ 
  def run = {
    val userOpts = this.parse
    anyToString(userOpts("platform")) match {
      case "solid" => processSolid(userOpts) 
    }
    println(ct_map)
  } 

  private def processSolid(userOpts: OptionMap) = {
    val infa = anyToFile(userOpts("incsfa"))
    val inq = anyToFile(userOpts("incsq"))
    ioInstance.assertFileIsReadable(infa)
    ioInstance.assertFileIsReadable(inq)
    val seqReader = ioInstance.openFileForBufferedReading(infa)
    val qualReader = ioInstance.openFileForBufferedReading(inq)
    val solidIter = CSFastaReader.parseCSFasta(
	seqReader, qualReader, infa, inq, 
	if (userOpts.isDefinedAt("start")) Some(anyToInt(userOpts("start"))) else None, 
	if (userOpts.isDefinedAt("end")) Some(anyToInt(userOpts("end"))) else None)
    for (rec <- solidIter){
      ct_map("Total Reads") += 1
      // check for N's
      if (rec.seqLine.contains("."))
        ct_map("Missing Base") += 1
      else {
        if (rec.averageQuality < anyToInt(userOpts("minq"))) 
          ct_map("Low Quality") += 1
        else {
          if (rec.isHomopolymer(anyToDbl(userOpts("hpoly"))))
            ct_map("Homopolymer") += 1
          else
            ct_map("Passed") += 1
        }
      }
    }
  }

  def anyToFile(a: Any) = a.asInstanceOf[File] 
  def anyToString(a: Any) = a.asInstanceOf[String] 
  def anyToInt(a: Any) = a.asInstanceOf[Int] 
  def anyToDbl(a: Any) = a.asInstanceOf[Double] 
}

