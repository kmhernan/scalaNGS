/**
 * Kyle Hernandez
 * FilterReads - package containing objects for filtering NGS reads
 *
 */

package com.tools.filters
import com.read.seq._
import scala.collection.mutable
import java.io._

object FilterRAD {
  // Globals
  val BASES = Array[Char]('A', 'C', 'G', 'T')
  type OptionMap = Map[Symbol, Any]
  var ct_map = scala.collection.mutable.Map[String, Int]("Total"->0, "LowQ"->0, "Hpoly"->0, "Passed"->0)

  def illuminaFilter(options: OptionMap): scala.collection.mutable.Map[String, Int] = {
    // Declare some variables for process and create output stream
    val ofile = new File(options('outfile).toString)
    val ostream = new java.io.PrintWriter(ofile)
    val qlim = options('qual).asInstanceOf[Int]
    val hlim = options('hpoly).asInstanceOf[Double]

    // Apply filters
    for (rec <- FastqTools.parseRadSE(options)) {
      ct_map("Total") += 1
      if (isHighQuality(rec, qlim))
        if (isHomopolymer(rec, hlim))
          ct_map("Hpoly") += 1
        else
          ct_map("Passed") += 1
      else
        ct_map("LowQ") += 1
    }
    ostream.close()
    return ct_map
  }

  def isHighQuality(rec: Fastq, qlimit: Int): Boolean = {
    if (rec.averageQuality >= qlimit)
      true
    else
      false
  }

  def isHomopolymer(rec: Fastq, hlimit: Double): Boolean = {
    val check = (hlimit * rec.sequence.length).toInt
    BASES.foreach{a => 
      if (rec.sequence.contains((a.toString)*check))
        return true
    }
    false 
  }

}
