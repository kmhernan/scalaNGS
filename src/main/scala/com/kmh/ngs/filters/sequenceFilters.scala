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

package com.kmh.ngs.filters
import com.kmh.ngs.formats._
import com.kmh.ngs.cmdline.{FilterSolidArgs}
import java.io.OutputStreamWriter
import scala.collection.mutable.{ListBuffer, Map}
import com.kmh.ngs.io.IoUtil

object SequenceFilters extends FilterSolidArgs with IoUtil {
  lazy val ct_map= Map[String, Int]( 
          "Total Reads"->0,
          "Homopolymer"->0,
          "Poly A"->0,
          "Too Short"->0,
          "Low Quality"->0,
          "Missing Base"->0,
          "Passed"->0)

  def isMissing(read: Read, userOpts: OptionMap): Boolean = {
    read match {
      case CSFastqRecord(_,seq,_,_) =>
        if (seq.contains(".")) {
          ct_map("Missing Base") += 1
          true
        } else false
      case FastqRecord(_,seq,_,_) =>
        if (seq.count(_ == 'N') > userOpts("minN").asInstanceOf[Int]) {
          ct_map("Missing Base") += 1
          true
        } else false
    }
  }

  def isLowQual(read: Read, userOpts: OptionMap): Boolean = {
    read match {
      case cs: CSFastqRecord(_,_,_,_) => {
        if (cs.averageQuality < getArg(userOpts('minq)) {
          ct_map("Low Quality") += 1
          true
        } else false
      }
      case fq: FastqRecord(_,seq,_,_) => {
        if (fq.averageQuality(userOpts("offset").asInstanceOf[Int]) < userOpts("minq").asInstanceOf[Int]) {
          ct_map("Low Quality") += 1
          true
        } else false
      }
    }
  }

  def isHomopolymer(read: Read, userOpts: OptionMap): Boolean = {
    read match {
      case CSFastaRecord(_,seq,_,_) =>
        lazy val checkString = "0" * (seq.length * getArg(userOpts('hpoly)).toInt
        if (seq.contains(checkString)) {
          ct_map("Homopolymer") += 1
          true
        } else false
      case FastqRecord(_,seq,_,_) =>
        lazy val basesArray = Array[String]("A", "C", "G", "T")
        if (basesArray.map(_*(seq.length*userOpts("hpoly").asInstanceOf[Double]).toInt).forall(seq.contains(_) == false))
          false
        else {
          ct_map("Homopolymer") += 1
          true
        }
    }
  }

  def apply(readIterator: , userOpts: OptionMap): List[((Read, OptionMap)) => Boolean] = {
    val filterFunctions = new ListBuffer[((Read, OptionMap)) => Boolean]
    if (userOpts.isDefinedAt('minN))
      filterFunctions += Function tupled isMissing _
    if (userOpts.isDefinedAt('hpoly))
      filterFunctions += Function tupled isHomopolymer _
    if (userOpts.isDefinedAt('minq))
      filterFunctions += Function tupled isLowQual _
    return filterFunctions.toList
  }

} 
