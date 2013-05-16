package com.kmh.ngs.analyses
import com.kmh.ngs.readers.SAMReader
import com.kmh.ngs.formats.SAMRecord
import scala.collection.mutable.{ArrayBuffer, Map}

/**
 * Represents a container for statistics specific to a given index in a FASTQ file.
 */
class SAMData {
  val mapqArray = new ArrayBuffer[Int]
  val queryArray = new ArrayBuffer[String] 
  var multiMap = Map[String, Int]() 
  var counts = 0

  def parse(x: SAMRecord): Unit = {
    counts += 1
    while (x.mapq >= mapqArray.length) {
      mapqArray += 0
    }
    mapqArray(x.mapq) += 1
    if (queryArray.exists(y => y == x.query)) {
      if (multiMap.isDefinedAt(x.query)) {
        multiMap(x.query) += 1
      } 
      else {
        multiMap += (x.query -> 0)
        multiMap(x.query) += 1
      }
    }
    else {
      queryArray += x.query
    }
  }

  lazy val sum: Int = mapqArray.view.zipWithIndex.foldLeft(0)((r,c) => r + c._1*c._2)
  lazy val mean: Double = sum / counts.toDouble
  def min: Int = mapqArray.takeWhile(x => x == 0).length
  def max: Int = mapqArray.length - mapqArray.reverse.takeWhile(x => x == 0).length - 1
  def med: Int = {
    val n = counts/2 
    var cts = 0
    mapqArray.takeWhile{x => cts += x; cts < n}.length - 1
  }
  def stdev = math.sqrt(mapqArray.view.zipWithIndex.filterNot{
    case(v, i) => v == 0}.foldLeft(0.0){
    case(r, (v,i)) => r + math.pow(i - mean, 2)*v} / (counts - 1.0))
}

object RunSAMStats {
  def apply(rr: SAMReader): Unit = {
    val samData = new SAMData
    rr.iter.foreach(rec => samData.parse(rec))
    println("%s\t%s\t%s\t%s\t%s\t%s\t%s".format(samData.sum, samData.mean, samData.stdev, 
	samData.min, samData.max, samData.med, samData.counts))
  } 
}
