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

package com.kmh.ngs.formats
import java.io.OutputStreamWriter

trait Read {
  val seqID: String
  val sequence: String
  val qualID: String
  val quality: String
  def averageQuality(qualityOffset: Option[Int]): Double 
  def writeToFile(ostream: List[OutputStreamWriter]): Unit
}

/**
 * Represents a Color-Space FASTA record
 *
 */
case class CSFastaRecord(
	val seqID: String, 
	val sequence: String,
	val qualID: String,
    	val quality: String) extends Read {

  /**
   * Writes the current record to a file
   *
   * @param ofa - file for csfasta reads
   * @param oq - file for quals
   */
  def writeToFile(ostream: List[OutputStreamWriter]): Unit = {
    ostream(0).write(seqID+"\n"+sequence+"\n")
    ostream(1).write(qualID+"\n"+quality+"\n")
  }

  def averageQuality(qualityOffset: Option[Int]): Double = 
        quality.split(" ").foldLeft(0)(_+_.toInt) / (sequence.length - 1).toDouble
}

case class FastqRecord(
        val seqID: String,
        val sequence: String,
        val qualID: String,
        val quality: String) extends Read {

  /**
    * Writes the current record to a file in Fastq format.
    *
    * @param ofq the output file represented as an [[java.io.OutputStreamWriter]]
    */
  def writeToFile(ostream: List[OutputStreamWriter]): Unit = {
    ostream(0).write(seqID + "\n" + sequence + "\n" +
                     qualID + "\n" + quality + "\n")
  }

  def averageQuality(qualityOffset: Option[Int]): Double = 
    qualityOffset match {
      case Some(qualityOffset) => 
        quality.foldLeft(0)(_+_.toInt - qualityOffset) / quality.length.toDouble
      case None => throw new Exception("Should never happen")
    }
}

case class PEFastqRecord(
        val seqID: String,
        val sequence: String,
        val qualID: String,
        val quality: String,
	val read2: FastqRecord) extends Read {
  /**
    * Writes the current record to a file in Fastq format.
    *
    * @param ofq the output file represented as an [[java.io.OutputStreamWriter]]
    */
  def writeToFile(ostream: List[OutputStreamWriter]): Unit = {
    if (ostream.length == 2) {
      ostream(0).write(seqID + "\n" + sequence + "\n" +
                       qualID + "\n" + quality + "\n")
      read2.writeToFile(List(ostream(1)))
    }
    else { 
      ostream(0).write(seqID + "\n" + sequence + "\n" +
                       qualID + "\n" + quality + "\n" +
                       read2.seqID + "\n" + read2.sequence + "\n" +
                       read2.qualID + "\n" + read2.quality + "\n")
    }
  }

  def averageQuality(qualityOffset: Option[Int]): Double = 
    qualityOffset match {
      case Some(qualityOffset) => quality.foldLeft(0)(_+_.toInt - qualityOffset) / quality.length.toDouble
      case None => throw new Exception("Should never happen")
    }
}  
