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
import com.kmh.ngs.formats.{Read, CSFastaRecord, FastqRecord, PEFastqRecord}
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

  /**
   * Checks if the read passes 'N' filters
   *
   * @param read an instance of [[com.kmh.ngs.formats.Read]]
   * @param userOpts the map of command-line arguments
   */ 
  def isMissing(read: Read, userOpts: OptionMap): Boolean = {
    read match {
      case cs: CSFastaRecord =>
        if (cs.sequence.contains(".")) {
          ct_map("Missing Base") += 1
          true
        } else false
      case fq: FastqRecord => {
        lazy val minN = userOpts('minN).asInstanceOf[Int]
        if (fq.sequence.count(_ == 'N') > minN) {
          ct_map("Missing Base") += 1
          true
        } else false
      }
      case pefq: PEFastqRecord => {
        lazy val minN = userOpts('minN).asInstanceOf[Int]
        if (pefq.sequence.count(_ == 'N') > minN || 
            pefq.read2.sequence.count(_ == 'N') > minN) { 
          ct_map("Missing Base") += 1
          true
        } else false
      }
    }
  }

  /**
   * Checks if the read passes quality filter 
   *
   * @param read an instance of [[com.kmh.ngs.formats.Read]]
   * @param userOpts the map of command-line arguments
   */ 
  def isLowQual(read: Read, userOpts: OptionMap): Boolean = {
    lazy val minq = userOpts('minq).asInstanceOf[Int]
    read match {
      case cs: CSFastaRecord => {
        if (cs.averageQuality(None) < minq) { 
          ct_map("Low Quality") += 1
          true
        } else false
      }
      case fq: FastqRecord => {
        lazy val offset = userOpts('offset).asInstanceOf[Int]
        if (fq.averageQuality(Some(offset)) < minq) {
          ct_map("Low Quality") += 1
          true
        } else false
      }
      case pefq: PEFastqRecord => {
        lazy val offset = userOpts('offset).asInstanceOf[Int]
        if (pefq.averageQuality(Some(offset)) < minq ||
            pefq.read2.averageQuality(Some(offset)) < minq) {
          ct_map("Low Quality") += 1
          true
        } else false
      }
    }
  }

  /**
   * Checks if the read is a homopolymer 
   *
   * @param read an instance of [[com.kmh.ngs.formats.Read]]
   * @param userOpts the map of command-line arguments
   */ 
  def isHomopolymer(read: Read, userOpts: OptionMap): Boolean = {
    lazy val basesArray = Array[String]("A", "C", "G", "T")
    lazy val hpoly = userOpts('hpoly).asInstanceOf[Double]
    read match {
      case cs: CSFastaRecord =>
        lazy val checkString = "0" * (cs.sequence.length * hpoly).toInt 
        if (cs.sequence.contains(checkString)) {
          ct_map("Homopolymer") += 1
          true
        } else false
      case fq: FastqRecord =>
        if (basesArray.map(_*(fq.sequence.length*hpoly).toInt).forall(fq.sequence.contains(_) == false))
          false
        else {
          ct_map("Homopolymer") += 1
          true
        }
      case pefq: PEFastqRecord =>
        if (basesArray.map(_*(pefq.sequence.length*hpoly).toInt).forall(pefq.sequence.contains(_) == false) &&
            basesArray.map(_*(pefq.read2.sequence.length*hpoly).toInt).forall(pefq.read2.sequence.contains(_) == false))
          false
        else {
          ct_map("Homopolymer") += 1
          true
        }
    }
  }

  /**
   * Removes polyA tail by making a copy of the immutable case class instance 
   *
   * @param read an instance of [[com.kmh.ngs.formats.Read]]
   * @param polyLimit the relative length of AAA tail to consider polyA
   * @return a copy of the [[com.kmh.ngs.formats.Read]] instance with polyA tail removed 
   */ 
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
    case pefq: PEFastqRecord => {
      lazy val paString = "A"*(pefq.sequence.length*polyLimit).toInt
      val paIndex = pefq.sequence.indexOf(paString)
      val pa2Index = pefq.read2.sequence.indexOf(paString)
      if (paIndex < 0 && pa2Index < 0)
        pefq 
      else {
        ct_map("Poly A") += 1
        pefq.copy(sequence = pefq.sequence.take(paIndex), quality = pefq.quality.take(paIndex), 
		read2 = pefq.read2.copy(sequence = pefq.read2.sequence.take(pa2Index), 
		quality = pefq.read2.quality.take(pa2Index)))
      }
    }
  }

  /**
   * Creates a list of tupled functions to apply to reads.
   *
   * @param userOpts the map of command-line arguments
   * @return [[scala.collection.List[(([[com.kmh.ngs.formats.Read]], Map[Symbol, Any])) => Boolean]
   */
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
    if(userOpts.isDefinedAt('polyA)) {
      lazy val paLimit = userOpts('polyA).asInstanceOf[Double]
      lazy val szLimit = userOpts('minSize).asInstanceOf[Int]
      readReader.iter.foreach(rec => {
        ct_map("Total Reads") += 1
        val recWithoutPoly = removePolyA(rec, paLimit)
        recWithoutPoly match {
          case fq: FastqRecord => 
            if (fq.sequence.length < szLimit)
              ct_map("Too Short") += 1
            else
              filterFunctions.find(_((fq, userOpts)) == true) match {
                case None => ct_map("Passed") += 1; fq.writeToFile(outList);
                case Some(_) => null
              }
          case pefq: PEFastqRecord => 
            if (pefq.sequence.length < szLimit || pefq.read2.sequence.length < szLimit)
              ct_map("Too Short") += 1
            else
              filterFunctions.find(_((pefq, userOpts)) == true) match {
                case None => ct_map("Passed") += 1; pefq.writeToFile(outList);
                case Some(_) => null
              }
        }
      })
    } else
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
