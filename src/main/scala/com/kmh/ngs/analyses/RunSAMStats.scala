package com.kmh.ngs.analyses
import com.kmh.ngs.readers.SAMReader
import com.kmh.ngs.formats.SAMRecord
import scala.collection.mutable.{Map, ListBuffer}
import org.eintr.loglady.Logging

/**
 * Provides containers for SAM file data and various statistics. 
 */
class SAMData {
  // Initialize class parameters
  val mapqArray = Array.fill(251)(0)
  val queryArray = new ListBuffer[String]
  val multiMap = Map[String, Int]() 
  var counts = 0
  var sum = 0

  /**
   * Parses the current SAMRecord instance into the containers and
   * increments counts.
   *
   * @param x an instance of a [[com.kmh.ngs.formats.SAMRecord]]
   */
  def parse(x: SAMRecord): Unit = {
    counts += 1
    sum += x.mapq

    try
      mapqArray(x.mapq) += 1
    catch {
      case iob: ArrayIndexOutOfBoundsException => 
        throw new RuntimeException("Quality scores exceed 250! This should not happen\n"+iob)
    }

    if (queryArray.exists(y => y == x.query)) {
      if (multiMap.isDefinedAt(x.query)) { multiMap(x.query) += 1 }
      else { multiMap += x.query -> 2 }
    }
    else queryArray += x.query
  }

  /**
   * Generates statistics and prints in nice format.
   */
  def getStatsReport: Unit = {
    val mean: Double = sum / counts.toDouble
    val min: Int = mapqArray.takeWhile(x => x == 0).length
    val max: Int = mapqArray.length - mapqArray.reverse.takeWhile(x => x == 0).length - 1
    val med: Int = {
      val n = counts / 2
      var cts = 0
      mapqArray.takeWhile{x => cts += x; cts < n}.length - 1 
    }
    val stdev: Double = math.sqrt(mapqArray.view.zipWithIndex.filterNot{
      case(v, i) => v == 0}.foldLeft(0.0){
      case(r, (v,i)) => r + math.pow(i - mean, 2)*v} / (counts - 1.0))
   println("N\tSum\tMean\tSD\tMin\tMax\tMed")
   println("%s\t%s\t%.2f\t%.2f\t%s\t%s\t%s".format(counts, sum, mean, stdev, min, max, med))
  }

} 

object RunSAMStats extends Logging {
  def apply(rr: SAMReader): Unit = {
    val samData = new SAMData
    log.info("Parsing SAM Records...")
    rr.iter.foreach(rec => samData.parse(rec))
    log.info("Generating statistics report...")
    samData.getStatsReport
  } 
}
