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
 * For more information, please refer to <http://unlicense.org/>
 *
 */

package com.kmh.ngs.cmdline
import org.eintr.loglady.Logging
import java.io.File

class ReadStatisticsArgs extends Arguments with Logging {
  val SP = " " * ("usage: java -jar NGSTools.jar ".length)
  val required = List('infq, 'offset)
  val basesArray = Array[String]("A", "C", "G", "T")
 
  def mainUsage = List(
    "usage: java -jar NGSTools.jar -T FilterReads -P/-PLATFORM SE_illumina ",
    SP+"-I/-INPUT file.fastq -O/-OUTPUT file.fastq -QV-OFFSET {33,64}",
    SP+"[-START Int] [-END Int] [-HPOLY Double] [-MINQ Int] [-NMISSING Int]",
    SP+"[-POLYA Double Int] [-h/--help]\n").map(println(_))

  def mainVerboseUsage = {
    mainUsage
    List("Required Arguments:",
      "  -I/-INPUT\tInput raw read files: <file.fastq> or <file.fastq.gz>",
      "  -QV-OFFSET\tPhred-scaled offset [33, 64]\n").map(println(_))
    List("Optional Arguments:",
      "  -MINQ\t\tMinimum average quality score allowed.",
      "  -h/--help\tPrint this message and exit.\n").map(println(_))
  }

  def checkRequired(map: OptionMap): OptionMap = {
    if (required.forall(x => map.isDefinedAt(x)))
      map
    else if (map.isEmpty) {
      mainUsage
      sys.exit(0)
    } else {
      mainUsage
      log.error(throw new IllegalArgumentException("Missing Required Arguments!!"))
      sys.exit(1)
    }
  }

  def parse(map: OptionMap, list: List[String]): OptionMap = {
    list match {
      case Nil => checkRequired(map)
      case "-I" :: file :: tail => parse(map ++ Map('infq-> new File(file)), tail)
      case "-INPUT" :: file :: tail => parse(map ++ Map('infq-> new File(file)), tail)
      case "-QV-OFFSET" :: value :: tail => parse(map ++ Map('offset->value.toInt), tail)
      case "-h" :: tail => mainVerboseUsage; sys.exit(0)
      case "--help" :: tail => mainVerboseUsage; sys.exit(0)
      case option => mainUsage;
                     log.error(throw new IllegalArgumentException("Unknown Option "+option));
                     sys.exit(1);
    }
  }

}

object ReadStatisticsArgs {
  type OptionMap = Map[Symbol, Any]

  def apply(args: List[String]): OptionMap = {
    val initIllumina = new ReadStatisticsArgs
    initIllumina.parse(Map(), args)
  }
}