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
import com.kmh.ngs.formats.{Read, CSFastaRecord, FastqRecord}
import com.kmh.ngs.readers.ReadReader
import java.io.OutputStreamWriter
import scala.collection.mutable.{ListBuffer, Map}

object SequenceFilters {
  type OptionMap = scala.collection.immutable.Map[Symbol, Any]
  lazy val ct_map = Map[String, Int]( 
          "Total Reads"->0,
          "Homopolymer"->0,
          "Poly A"->0,
          "Too Short"->0,
          "Low Quality"->0,
          "Missing Base"->0,
          "Passed"->0)

  def isMissing(read: Read, userOpts: OptionMap): Boolean = {
    read match {
      case cs: CSFastaRecord =>
        if (cs.sequence.contains(".")) {
          ct_map("Missing Base") += 1
          true
        } else false
      case fq: FastqRecord =>
        if (fq.sequence.count(_ == 'N') > userOpts('minN).asInstanceOf[Int]) {
          ct_map("Missing Base") += 1
          true
        } else false
    }
  }

  def isLowQual(read: Read, userOpts: OptionMap): Boolean = {
    read match {
      case cs: CSFastaRecord => {
        if (cs.averageQuality(None) < userOpts('minq).asInstanceOf[Int]) {
          ct_map("Low Quality") += 1
          true
        } else false
      }
      case fq: FastqRecord => {
        if (fq.averageQuality(Some(userOpts('offset).asInstanceOf[Int])) < userOpts('minq).asInstanceOf[Int]) {
          ct_map("Low Quality") += 1
          true
        } else false
      }
    }
  }

  def isHomopolymer(read: Read, userOpts: OptionMap): Boolean = {
    read match {
      case cs: CSFastaRecord =>
        lazy val checkString = "0" * (cs.sequence.length * userOpts('hpoly).asInstanceOf[Double]).toInt
        if (cs.sequence.contains(checkString)) {
          ct_map("Homopolymer") += 1
          true
        } else false
      case fq: FastqRecord =>
        lazy val basesArray = Array[String]("A", "C", "G", "T")
        if (basesArray.map(_*(fq.sequence.length*userOpts('hpoly).asInstanceOf[Double]).toInt).forall(fq.sequence.contains(_) == false))
          false
        else {
          ct_map("Homopolymer") += 1
          true
        }
    }
  }

  def removePolyA(rec: Read, polyLimit: Double): Read = rec match {
    case fq: FastqRecord => {
      lazy val paString = "A"*(fq.sequence.length*polyLimit).toInt
      val paIndex = fq.sequence.indexOf(paString)
      if (paIndex < 0)
        fq 
      else {
        ct_map("Poly A") += 1
        fq.copy(sequence = fq.sequence.take(paIndex), quality = fq.quality.take(paIndex))
      }
    }
  }

  def loadFilters(userOpts: OptionMap): List[((Read, OptionMap)) => Boolean] = {
    val filterFunctions = new ListBuffer[((Read, OptionMap)) => Boolean]
    if (userOpts.isDefinedAt('minN))
      filterFunctions += Function tupled isMissing _
    if (userOpts.isDefinedAt('hpoly))
      filterFunctions += Function tupled isHomopolymer _
    if (userOpts.isDefinedAt('minq))
      filterFunctions += Function tupled isLowQual _
    return filterFunctions.toList
  }

  def apply(readReader: ReadReader, userOpts: OptionMap, outList: List[OutputStreamWriter]): Map[String, Int] = {
    val filterFunctions = loadFilters(userOpts)
    if(userOpts.isDefinedAt('polyA))
      readReader.iter.foreach(rec => {
        ct_map("Total Reads") += 1
        val recWithoutPoly = removePolyA(rec, userOpts('polyA).asInstanceOf[Double])
        if (recWithoutPoly.sequence.length < userOpts('minSize).asInstanceOf[Int])
          ct_map("Too Short") += 1
        else
          filterFunctions.find(_((recWithoutPoly, userOpts)) == true) match {
            case None => ct_map("Passed") += 1; recWithoutPoly.writeToFile(outList);
            case Some(_) => null
          }
      })
    else 
      readReader.iter.foreach(rec => {
        ct_map("Total Reads") += 1
        filterFunctions.find(_((rec, userOpts)) == true) match {
          case None => ct_map("Passed") += 1; rec.writeToFile(outList);
          case Some(_) => null
        }
      })
    ct_map
  }

} 
