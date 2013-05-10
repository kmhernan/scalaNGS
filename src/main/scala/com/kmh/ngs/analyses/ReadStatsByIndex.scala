package com.kmh.ngs.analyses
import com.kmh.ngs.readers.ReadReader
import org.eintr.loglady.Logging

class ReadIndexData {
  val quality_container = Array.fill(50)(0)
  val nucleotide_container = Array.fill(5)(0)
  var counts = 0
  var max = 0
  var min = 0

  lazy val sum: Int = quality_container.view.zipWithIndex.foldLeft(0)((r,c) => r + c._1*c._2)

  lazy val q1: Int = {
    var pos = 0; val n = counts/4; var sum = 0
    while (sum < n && quality_container(pos) <= n) {
      pos += 1
      sum += quality_container(pos)
    }
    pos
  }

  lazy val q3: Int = {
    var pos = 0; val n = counts * 3 / 4; var cts = 0
    while (cts < n && quality_container(pos) <= n) {
      pos += 1
      cts += quality_container(pos)
    }
    pos
  }

  lazy val med: Int = {
    var pos = 0; val n = counts/2; var cts = 0
    while (cts < n && quality_container(pos) <= n) {
      pos += 1
      cts += quality_container(pos)
    }
    pos
  }

  def addQ(i: Int): Unit = {
    quality_container(i) += 1
    if (counts == 0) min = i else if (i < min) min = i
    if (i > max) max = i
    counts += 1
  }

  def addN(i: Int): Unit = nucleotide_container(i) += 1
  def mean: Double = sum / counts.toDouble
  def iqr: Int = q3 - q1
  def nA: Int = nucleotide_container(0) 
  def nC: Int = nucleotide_container(1) 
  def nG: Int = nucleotide_container(2) 
  def nT: Int = nucleotide_container(3) 
  def nN: Int = nucleotide_container(4) 
}

/**
 * Calculates quality statistics and base frequencies at each position in a sequence
 *
 */
object ReadStatsByIndex extends Logging {
  val baseToInt = Array[Char]('A', 'C', 'G', 'T', 'N') 
  val readArray = new scala.collection.mutable.ArrayBuffer[ReadIndexData]

  def apply(rr: ReadReader, offset: Int): Array[ReadIndexData] = {
    rr.iter.foreach(rec => {
      rec.quality.map(_.toInt-offset).view.zipWithIndex.foreach{
        case(v,i) => { 
          if (readArray.isEmpty)
            for (i<- 0 until rec.quality.length) {
              readArray += new ReadIndexData
            }
          readArray(i).addQ(v)
          readArray(i).addN(baseToInt.indexOf(rec.sequence(i)))
        }
      }
    })
    readArray.toArray
  } 

}
