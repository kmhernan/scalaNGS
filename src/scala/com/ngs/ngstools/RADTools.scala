/**
 * Insert license here
 *
 */

//package com.ngs.ngstools
//import com.ngs.io.IoUtil

/**
 * Command line tool for filtering NGS Fasta/Fastq files.
 *
 */

object RADTools { 
 
  // Define command-line arguments
  val USAGE = "TEST"
  val  
  def main(args: Array[String]): Unit = {
    val optsMap = Map() ++ args.map{ x => 
      val pair = x.split("=")
      if(pair.length ==1) (pair(0), "true") else (pair(0), pair(1))
    }  
    println(optsMap) 
  }
}
