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
   * Returns the average quality of the current record
   *
   * @return the average quality of the read.
   */
  override def averageQuality: Double = {
    quality.split(" ").foldLeft(0)(_+_.toInt) / (sequence.length - 1).toDouble
  }

  /**
   * Writes the current record to a file
   *
   * @param ofa - file for csfasta reads
   * @param oq - file for quals
   */
  def writeToFile(ofa: OutputStreamWriter, oq: OutputStreamWriter): Unit = {
    ofa.write(seqID+"\n")
    ofa.write(sequence+"\n")
    oq.write(qualID+"\n")
    oq.write(quality+"\n")
  }

}

case class FastqRecord(
        val seqHeader: String,
        val seqLine: String,
        val qualHeader: String,
        val qualLine: String) extends Read {
  /**
   * Get the average Phred-scaled quality score based on the read offset.
   *
   * @param qv_offset the quality offset (33/64)
   * @returns average quality [Double]
   */
  def averageQuality(qv_offset: Int): Double = {
    qualLine.foldLeft(0)(_+_.toInt - qv_offset) / qualLine.length.toDouble
  }

  /**
    * Provides the barcode of the current record.
    *
    * @return barcode the string representation of the barcode
    */
  def barcode: String = seqHeader.split(":").toList.reverse.head

  /**
    * Writes the current record to a file in Fastq format.
    *
    * @param ofq the output file represented as an [[java.io.OutputStreamWriter]]
    */
  def writeToFile(ofq: OutputStreamWriter): Unit = {
    ofq.write(this.seqHeader + "\n" + this.seqLine + "\n" +
              this.qualHeader + "\n" + this.qualLine + "\n")
  }
}
