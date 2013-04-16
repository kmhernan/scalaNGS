/**
 * Kyle Hernandez
 * FilterReads - package containing objects for filtering NGS reads
 *
 */

package com.tools.filters
import com.read._
import scala.collection.mutable
import java.io._

object FilterRAD {

  type OptionMap = Map[Symbol, Any]
  var ct_map = scala.collection.mutable.Map[String, Int]("Total"->0, "LowQ"->0, "Hpoly"->0, "Passed"->0)

  def illuminaFilter(options: OptionMap): CTMap = {
    val ifile = new File(options('infile).toString)
    val ofile = new File(options('outfile).toString)
    val ostream = new java.io.PrintWriter(ofile)

    for {rec <-  
