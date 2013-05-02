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
        "Low Quality"->0,
        "Missing Base"->0,
        "Passed"->0)
  private val mainUsage = 
    "usage: java -jar NGSTools.jar -T FilterReads -P/-PLATFORM illumina -I/-INPUT file.fastq\n" +
    "-O/-OUTPUT file.fastq [-START Int] [-END Int] [-HPOLY Double] [-MINQ Int] [-NMISSING Int]\n" +
    "[-POLYA Double] [-h/--help]\n"
  private val mainVerboseUsage = mainUsage +
    "REQUIRED:\n" +
    "  -I/-INPUT\tInput raw read files: <file.fastq>|<file.fastq.gz>\n" +
    "  -O/-OUTPUT\tOutput filtered read files: <file.fastq>\n" +
    "  -QV-OFFSET\tPhred-scaled offset [33, 64]\n\n" +
    "OPTIONAL (Automatically removed reads with missing bases):\n" +
    "  -START\t5' cut position (1-based index)\n" +
    "  -END\t3' cut position (1-based index)\n" +
    "      \tex. AlfI: -START 1 -END 36\n" +
    "  -HPOLY\tRelative length of repetitive base to consider a homopolymer.\n" +
    "        \t(Proportion of read length; e.g., between 0 and 1)\n" +
    "  -MINQ\tMinimum average quality score allowed.\n" +
    "  -NMISSING\tLower limit for N's allowed.\n" + 
    "  -POLYA\tIf a read has trailing A's of length <input> * sequence length, trim them.\n" +
    "  -h/--help\tPrint this message and exit.\n"
  private val required = List("infq", "outfq", "offset")
  private val ioInstance = new IoUtil
  private val filterFunctions = new ListBuffer[((FastqRecord, OptionMap)) => Boolean]
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
     * Parses Illumina arguments
     *
     * @param OptionMap
     * @param a list of the arguments
     * @return OptionMap
     */
  private def parseIllumina(map: OptionMap, list: List[String]): OptionMap = {
    list match {
      case Nil => checkRequired(map)
      case "-I" :: file1 :: tail => parseIllumina(map ++ Map("infq"-> new File(file1)), tail)
      case "-INPUT" :: file1 :: tail => parseIllumina(map ++ Map("infq"-> new File(file1)), tail)
      case "-O" :: file1 :: tail => parseIllumina(map ++ Map("outfq"-> new File(file1)), tail)
      case "-OUTPUT" :: file1 :: tail => parseIllumina(map ++ Map("outfq"-> new File(file1)), tail)
      case "-QV-OFFSET" :: value :: tail => parseIllumina(map ++ Map("offset"->value.toInt), tail)
      case "-START" :: value :: tail => parseIllumina(map ++ Map("start"->value.toInt), tail)
      case "-END" :: value :: tail => parseIllumina(map ++ Map("end"->value.toInt), tail)
      case "-HPOLY" :: value :: tail => parseIllumina(map ++ Map("hpoly"->value.toDouble), tail)
      case "-MINQ" :: value :: tail => parseIllumina(map ++ Map("minq"->value.toInt), tail)
      case "-NMISSING" :: value :: tail => parseIllumina(map ++ Map("minN"->value.toInt), tail)
      case "-POLYA" :: value :: tail => parseIllumina(map ++ Map("polyA"->value.toDouble), tail)
      case "-h" :: value :: tail => log.info(mainVerboseUsage); sys.exit(0);
      case "--help" :: value :: tail => log.info(mainVerboseUsage); sys.exit(0);
      case option => log.warn(mainUsage);
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
    if (required.forall(x => map.isDefinedAt(x))) 
      map
    else { 
      log.warn(mainUsage)
      log.error(throw new IllegalArgumentException("Missing Required Arguments!!"))
      sys.exit(1)
    }
  }

  def filterOptions(userOpts: OptionMap) = {
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
  }

  def isMissing(rec: FastqRecord, userOpts: OptionMap) = {
    if (rec.seqLine.count(_ == 'N') > anyToInt(userOpts("minN"))){
      ct_map("Missing Base") += 1
      true
    } else false
  }

  def isLowQual(rec: FastqRecord, userOpts: OptionMap) = {
    if (rec.averageQuality(anyToInt(userOpts("minq"))) < anyToInt(userOpts("minq"))) {
      ct_map("Low Quality") += 1
      true
    } else false
  }

  def isHomopolymer(rec: FastqRecord, userOpts: OptionMap) = {
    if (basesArray.map(_*(rec.seqLine.length*anyToDbl(userOpts("hpoly"))).toInt).forall(rec.seqLine.contains(_) == false))
      false
    else {
      ct_map("Homopolymer") += 1
      true
    }
  }

  def main(args: List[String]): Unit = {
    val userOpts = parseIllumina(Map(), args)
    val infq  = anyToFile(userOpts("infq"))
    val outfq = anyToFile(userOpts("outfq"))
    ioInstance.assertFileIsReadable(infq)
    val seqReader  = ioInstance.openFileForBufferedReading(infq)
    val seqWriter = ioInstance.openFileForWriting(outfq)

    log.info("Processing Illumina reads...")
    try {
      filterOptions(userOpts)
      val filtList = filterFunctions.toList

      //if(userOpts.isDefinedAt("polyA"))
      //  val illuminaIter = parsePoly(seqReader, infq, userOpts)
      //else
        val illuminaIter = FastqReader.parseFastq(
        	seqReader, infq,
        	if (userOpts.isDefinedAt("start")) Some(anyToInt(userOpts("start"))) else None,
        	if (userOpts.isDefinedAt("end")) Some(anyToInt(userOpts("end"))) else None)

      illuminaIter.foreach(x => {
        ct_map("Total Reads") += 1
        if (filtList.map(Map(x -> userOpts) map _ ).flatten.forall( _ == false )){
          ct_map("Passed") += 1
          x.writeToFile(seqWriter)
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
    log.info("TOTAL="+ct_map("Total Reads")+" "+
             "MISSING="+ct_map("Missing Base")+" "+
             "LOWQ="+ct_map("Low Quality")+" "+
             "HOMOPOLYMER="+ct_map("Homopolymer")+" "+
             "PASSED="+ct_map("Passed"))
  }

} 