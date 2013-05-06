/**
 * This is free and unencumbered software released into the public domain.
 *
 * Anyone is free to copy, modify, publish, use, compile, sell, or
 * distribute this software, either in source code form or as a compiled
 * binary, for any purpose, commercial or non-commercial, and by any
 * means.
 *
 * In jurisdictions that recognize copyright laws, the author or authors
 * of this software dedicate any and all copyright interest in the
 * software to the public domain. We make this dedication for the benefit
 * of the public at large and to the detriment of our heirs and
 * successors. We intend this dedication to be an overt act of
 * relinquishment in perpetuity of all present and future rights to this
 * software under copyright law.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
 * IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR
 * OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
 * ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
 * OTHER DEALINGS IN THE SOFTWARE.
 *
 *
 * For more information, please refer to <http://unlicense.org/>
 *
 */

package com.kmh.ngs.filters

import com.kmh.ngs.io.IoUtil
import com.kmh.ngs.readers._
import java.io.{File, OutputStreamWriter, BufferedReader, IOException}
import scala.collection.mutable.{Map, ListBuffer}
import org.eintr.loglady.Logging

/**
 * Filters Illumina reads
 *
 */
object illuminaFilters extends Logging {
  type OptionMap = Map[String, Any]
  private var ct_map = 
    scala.collection.mutable.Map[String, Int](
        "Total Reads"->0,
        "Homopolymer"->0,
        "Poly A"->0,
	"Too Short"->0,
        "Low Quality"->0,
        "Missing Base"->0,
        "Passed"->0)
  private val SP = " "*("Usage: java -jar NGSTools.jar -T FilterReads -P/-PLATFORM illumina ".length)
  private def mainUsage = List(
    "\nusage: java -jar NGSTools.jar -T FilterReads -P/-PLATFORM illumina -I/-INPUT file.fastq -O/-OUTPUT file.fastq -QV-OFFSET",
    SP + "[-START Int] [-END Int] [-HPOLY Double] [-MINQ Int] [-NMISSING Int]",
    SP + "[-POLYA Double Int] [-h/--help]\n").map(println(_))
  private def mainVerboseUsage = {
    mainUsage
    List("Required Arguments:",
      "  -I/-INPUT\tInput raw read files: <file.fastq> or <file.fastq.gz>",
      "  -O/-OUTPUT\tOutput filtered read files: <file.fastq>",
      "  -QV-OFFSET\tPhred-scaled offset [33, 64]\n").map(println(_))
    List("Optional Arguments:",
      "  -START\t5' cut position (1-based index)",
      "  -END\t\t3' cut position (1-based index)",
      "      \t\tex. AlfI: -START 1 -END 36",
      "  -HPOLY\tRelative length of repetitive base to consider a homopolymer. (Proportion of read length; e.g., between 0 and 1)",
      "  -MINQ\t\tMinimum average quality score allowed.",
      "  -NMISSING\tLower limit for N's allowed.", 
      "  -POLYA\tTakes two values:",
      "        \t  1) ProportionLimit [Double] - If a read has trailing A's of length <value> * sequence length, trim them.",
      "        \t  2) MinimumSize [Int] - If the trimmed sequence is shorter than <value>, remove it.",
      "  -h/--help\tPrint this message and exit.\n").map(println(_))
  }
  private val required = List("infq", "outfq", "offset")
  private val ioInstance = new IoUtil
  private val basesArray = Array[String]("A", "C", "G", "T")

  /** Convert [[Any]] into [[java.io.File]]*/
  def anyToFile(a: Any) = a.asInstanceOf[File]
  /** Convert [[Any]] into [[String]]*/
  def anyToString(a: Any) = a.asInstanceOf[String]
  /** Convert [[Any]] into [[Double]]*/
  def anyToDbl(a: Any) = a.asInstanceOf[Double]
  /** Convert [[Any]] into [[Int]]*/
  def anyToInt(a: Any) = a.asInstanceOf[Int]
 
   /**
     * Parses Illumina command-line arguments
     *
     * @param OptionMap
     * @param a list of the arguments
     * @return OptionMap
     */
  private def parseIllumina(map: OptionMap, list: List[String]): OptionMap = {
    list match {
      case Nil => checkRequired(map)
      case "-I" :: file :: tail => parseIllumina(map ++ Map("infq"-> new File(file)), tail)
      case "-INPUT" :: file :: tail => parseIllumina(map ++ Map("infq"-> new File(file)), tail)
      case "-O" :: file :: tail => parseIllumina(map ++ Map("outfq"-> new File(file)), tail)
      case "-OUTPUT" :: file :: tail => parseIllumina(map ++ Map("outfq"-> new File(file)), tail)
      case "-QV-OFFSET" :: value :: tail => parseIllumina(map ++ Map("offset"->value.toInt), tail)
      case "-START" :: value :: tail => parseIllumina(map ++ Map("start"->value.toInt), tail)
      case "-END" :: value :: tail => parseIllumina(map ++ Map("end"->value.toInt), tail)
      case "-HPOLY" :: value :: tail => parseIllumina(map ++ Map("hpoly"->value.toDouble), tail)
      case "-MINQ" :: value :: tail => parseIllumina(map ++ Map("minq"->value.toInt), tail)
      case "-NMISSING" :: value :: tail => parseIllumina(map ++ Map("minN"->value.toInt), tail)
      case "-POLYA" :: value :: value2 :: tail => 
	try
 	  parseIllumina(map ++ Map("polyA"->value.toDouble, "minSize"->value2.toInt), tail)
  	catch {
  	  case err: Throwable => 
		log.error("POLYA takes 2 values: ProportionLimit [Double] MinimumSize [Int]"+err);
		sys.exit(1)
      	} 
      case "-h" :: tail => mainVerboseUsage; sys.exit(0)
      case "--help" :: tail => mainVerboseUsage; sys.exit(0)
      case option => mainUsage;
                     log.error(throw new IllegalArgumentException("Unknown Option "+option));
                     sys.exit(1);
    }
  } 

  /**
    * Checks if all the required arguments are declared by the user.
    *
    * @param map the [[OptionMap]] of command-line arguments
    * @return an [[OptionMap]] with all required arguments
    * @throws [[IllegalArgumentException]]
    */
  private def checkRequired(map: OptionMap): OptionMap = {
    if (map.isEmpty){
      mainUsage
      sys.exit(0)
    }
    else if (required.forall(x => map.isDefinedAt(x))) 
      map
    else {
      mainUsage
      log.error(throw new IllegalArgumentException("Missing Required Arguments!!"))
      sys.exit(1)
    }
  }

  /**
    * Builds a list of filtering functions based on the command-line arguments.
    * The filtering functions are converted into the tupled functions for easier mapping.
    *
    * @param userOpts an [[OptionMap]] of command-line arguments
    * @return [[List[((FastqRecord, OptionMap))]]]
    */
  private def filterOptions(userOpts: OptionMap): List[((FastqRecord, OptionMap)) => Boolean] = {
    val filterFunctions = new ListBuffer[((FastqRecord, OptionMap)) => Boolean]
    if (userOpts.isDefinedAt("minN")){
      val tupMiss = Function tupled isMissing _
      filterFunctions += tupMiss
    }
    if (userOpts.isDefinedAt("hpoly")){
      val tupHomo = Function tupled isHomopolymer _
      filterFunctions += tupHomo
    }
    if (userOpts.isDefinedAt("minq")){
      val tupLow = Function tupled isLowQual _
      filterFunctions += tupLow
    }
    filterFunctions.toList
  }

  /**
    * Tests if the sequence has more than the number of N's allowed.
    *
    * @param rec a [[FastqRecord]] instance
    * @param userOpts an [[OptionMap]] of command-line arguments
    * @return [[Boolean]] true if fails else false
    */
  private def isMissing(rec: FastqRecord, userOpts: OptionMap) = {
    if (rec.seqLine.count(_ == 'N') > anyToInt(userOpts("minN"))){
      ct_map("Missing Base") += 1
      true
    } else false
  }

  /**
    * Tests whether the average quality score is below the minimum quality threshold. 
    *
    * @param rec a [[FastqRecord]] instance
    * @param userOpts an [[OptionMap]] of command-line arguments
    * @return [[Boolean]] true if fails else false
    */
  def isLowQual(rec: FastqRecord, userOpts: OptionMap) = {
    if (rec.averageQuality(anyToInt(userOpts("offset"))) < anyToInt(userOpts("minq"))) {
      ct_map("Low Quality") += 1
      true
    } else false
  }

  /**
    * Tests whether the sequence contains repetitive bases. 
    *
    * @param rec a [[FastqRecord]] instance
    * @param userOpts an [[OptionMap]] of command-line arguments
    * @return [[Boolean]] true if fails else false
    */
  def isHomopolymer(rec: FastqRecord, userOpts: OptionMap) = {
    if (basesArray.map(_*(rec.seqLine.length*anyToDbl(userOpts("hpoly"))).toInt).forall(rec.seqLine.contains(_) == false))
      false
    else {
      ct_map("Homopolymer") += 1
      true
    }
  }

  /**
    * Checks if the sequence has a polyA tail, removes it if it does.
    *
    * @param rec a [[FastqRecord]] instance
    * @param polyLimit the user-provided relative length of A sequences to be considered a polyA tail
    * @return [[FastqRecord]]
    */
  private def removePolyA(rec: FastqRecord, polyLimit: Double): FastqRecord = {
    lazy val paString = "A"*(rec.seqLine.length*polyLimit).toInt
    val paIndex = rec.seqLine.indexOf(paString)
    if (paIndex < 0)
      rec
    else {
      ct_map("Poly A") += 1
      rec.copy(seqLine = rec.seqLine.take(paIndex), qualLine = rec.qualLine.take(paIndex))
    }
  }
    
  def main(args: List[String], isPaired: Boolean): Unit = {
    val userOpts = parseIllumina(Map(), args)
    val infq  = anyToFile(userOpts("infq"))
    val outfq = anyToFile(userOpts("outfq"))
    ioInstance.assertFileIsReadable(infq)
    val seqReader  = ioInstance.openFileForBufferedReading(infq)
    val seqWriter = ioInstance.openFileForWriting(outfq)

    log.info("Processing Illumina reads...")
    try {
      val filterList = filterOptions(userOpts) 
      val illuminaIter = FastqReader.parseFastq(
      	seqReader, infq,
       	if (userOpts.isDefinedAt("start")) Some(anyToInt(userOpts("start"))) else None,
        if (userOpts.isDefinedAt("end")) Some(anyToInt(userOpts("end"))) else None)

      if(userOpts.isDefinedAt("polyA"))
        illuminaIter.foreach(rec => {
          ct_map("Total Reads") += 1
          val recWithoutPoly = removePolyA(
		rec, 
		anyToDbl(userOpts("polyA"))) 
	  if (recWithoutPoly.seqLine.length < anyToInt(userOpts("minSize")))
            ct_map("Too Short") += 1
          else 
	    filterList.find(_((recWithoutPoly, userOpts)) == true) match { 
                case None => ct_map("Passed") +=1; recWithoutPoly.writeToFile(seqWriter)
                case Some(_) => null 
            }
        }) 
      else 
        illuminaIter.foreach(rec => {
          ct_map("Total Reads") += 1
          filterList.find(_((rec,userOpts)) == true) match {
            case None => ct_map("Passed") +=1; rec.writeToFile(seqWriter)
            case Some(_) => null 
          }
        }) 
    }
    catch {
      case err: Throwable => List(seqReader, seqWriter).map(ioInstance.closer(_));
	log.error("Something went wrong "+err); sys.exit(1);
    } 
    finally {
      List(seqReader, seqWriter).map(ioInstance.closer(_))
    }
    List(seqReader, seqWriter).map(ioInstance.closer(_))
    log.info("FILE="+infq.getName()+" "+
             "TOTAL="+ct_map("Total Reads")+" "+
             "MISSING="+ct_map("Missing Base")+" "+
             "LOWQ="+ct_map("Low Quality")+" "+
             "HOMOPOLYMER="+ct_map("Homopolymer")+" "+
             "POLYA="+ct_map("Poly A")+" "+
             "TOOSHORT="+ct_map("Too Short")+" "+
             "PASSED="+ct_map("Passed"))
  }

} 
