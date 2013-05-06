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

case class FilterSolidArgs extends Arguments with Logging {
  val SP = " " * ("Usage: java -jar NGSTools.jar -T FilterReads ".length)
  val required = List('incsfa, 'incsq, 'ocsfa, 'ocsq)
  
  def mainUsage = List(
    "usage: java -jar NGSTools.jar -T FilterReads -P/-PLATFORM solid -I/-INPUT file.csfasta file.qual",
    SP+"-O/-OUTPUT file.csfasta file.qual [-START Int] [-END Int]",
    SP+"[-HPOLY Double] [-MINQ Int] [-h/--help]\n").map(println(_))

  def mainVerboseUsage = {
    mainUsage
    List(
      "Required Arguments:\n",
      "  -I/-INPUT"+"\t"+"Input raw read files: <file.csfasta> <file.qual>",
      "  -O/-OUTPUT"+"\t"+"Output filtered read files: <file.csfasta> <file.qual>\n").map(println(_))
    List(
      "Optional Arguments:\n",
      "  -START\t5' cut position (1-based index)",
      "  -END\t\t3' cut position (1-based index)",
      "  -HPOLY\tRelative length of repetitive base to consider a homopolymer.",
      "  -MINQ\tMinimum average quality score allowed.",
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

  def parse(map: OptionMap, list: List[String]): OptionMap =
    list match {
      case Nil => checkRequired(map)
      case "-h" :: tail => mainVerboseUsage; sys.exit(0)
      case "--help" :: tail => mainVerboseUsage; sys.exit(0)
      case "-I" :: file1 :: file2 :: tail =>
        if (file2.startsWith("-")) {
          log.error(throw new IllegalArgumentException("SOLiD reads expect 2 input and output files"))
          sys.exit(1)
        } else 
          parse(map ++ Map('incsfa -> new File(file1), 'incsq -> new File(file2)), tail)
      case "-INPUT" :: file1 :: file2 :: tail =>
        if (file2.startsWith("-")) {
          log.error(throw new IllegalArgumentException("SOLiD reads expect 2 input and output files"))
          sys.exit(1)
        } else 
          parse(map ++ Map('incsfa -> new File(file1), 'incsq -> new File(file2)), tail)
      case "-O" :: file1 :: file2 :: tail =>
        if (file2.startsWith("-")) {
          log.error(throw new IllegalArgumentException("SOLiD reads expect 2 input and output files"))
          sys.exit(1)
        } else 
          parse(map ++ Map('ocsfa -> new File(file1), 'ocsq -> new File(file2)), tail)
      case "-OUTPUT" :: file1 :: file2 :: tail =>
        if (file2.startsWith("-")) {
          log.error(throw new IllegalArgumentException("SOLiD reads expect 2 input and output files"))
          sys.exit(1)
        } else 
          parse(map ++ Map('ocsfa -> new File(file1), 'ocsq -> new File(file2)), tail)
      case "-START" :: value :: tail => parse(map ++ Map('start ->value.toInt), tail)
      case "-END" :: value :: tail => parse(map ++ Map('end ->value.toInt), tail)
      case "-HPOLY" :: value :: tail => parse(map ++ Map('hpoly ->value.toDouble), tail)
      case "-MINQ" :: value :: tail => parse(map ++ Map('minq ->value.toInt), tail)
      case "--MISSING" :: value :: tail => parse(map ++ Map('missing ->true), tail)
      case option => mainUsage; 
                     log.error(throw new IllegalArgumentException("Unknown Option "+option));
                     sys.exit(1)
    }
}

object FilterSolidArgs {
  type OptionMap = Map[Symbol, Any]
 
  def apply(args: List[String]): OptionMap = {
    val initSolid = new FilterSolidArgs
    initSolid.parse(Map(), args)
  }
} 
