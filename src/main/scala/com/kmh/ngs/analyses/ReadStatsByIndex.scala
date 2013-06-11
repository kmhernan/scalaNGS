package com.kmh.ngs.analyses
import com.kmh.ngs.readers.ReadReader

/**
 * Represents a container for statistics specific to a given index in a FASTQ file.
 */
class ReadIndexData {
  val quality_container = Array.fill(50)(0)
  val nucleotide_container = Array.fill(5)(0)
  var counts = 0
  var sum = 0

  lazy val mean: Double = sum / counts.toDouble
  def min: Int = quality_container.takeWhile(x => x == 0).length
  def max: Int = quality_container.length - quality_container.reverse.takeWhile(x => x == 0).length - 1
  def med: Int = {
    val n = counts/2 
    var cts = 0
    quality_container.takeWhile{x => cts += x; cts < n}.length - 1
  }

  def addQ(i: Int): Unit = {
    quality_container(i) += 1
    counts += 1
    sum += i
  }

  def addN(i: Int): Unit = nucleotide_container(i) += 1
  def stdev = math.sqrt(quality_container.view.zipWithIndex.filterNot{
    case(v, i) => v == 0}.foldLeft(0.0){
    case(r, (v,i)) => r + math.pow(i - mean, 2)*v} / (counts - 1.0))
  def nA: Int = nucleotide_container(0) 
  def nC: Int = nucleotide_container(1) 
  def nG: Int = nucleotide_container(2) 
  def nT: Int = nucleotide_container(3) 
  def nN: Int = nucleotide_container(4) 
}

/**
 * Wrapper to create an array of index-specific site statistics 
 *
 * @param rr the [[com.kmh.ngs.readers.ReadReader]] instance
 * @param offset the Phred-score offset
 * @return [[Array[com.kmh.ngs.analyses.ReadIndexData]]
 */
object ReadStatsByIndex {
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
