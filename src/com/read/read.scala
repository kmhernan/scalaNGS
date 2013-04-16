/** 
 * Kyle Hernandez
 * kmhernan@utexas.edu
 * Package: com.read.Read - contains classes, traits, modules, for NGS reads
 *
 */

package com.read.seq

import scala.io.Source
import java.io._
import java.util.zip.GZIP.InputStream

// Traits all NGS sequence classes inherit
trait Read {
  def name: String
  def sequence: String
  def quality: String
  def repr: Unit
}

/**
 * @class Fastq @extends Read
 * @input name: String 		- read name
 * @input sequence: String	- read sequence
 * @input opt: String		- read optional header
 * @input quality: String	- read quality
 * @function averageQuality 	@returns average quality (Float)
 * @function repr		- print representation of instance (Unit)
 * @function getBarcode		@returns barcode of read (String)
 */

class Fastq(val name: String, val sequence: String, 
            val opt: String, val quality: String,
            val offset: Int) extends Read {

  def averageQuality: Float={
    val test: Array[Int] = this.quality.map(_.toInt - this.offset).toArray
    return test.sum.toFloat / test.length
  }

  def repr: Unit={
    println("Name: " + name)
    println("Sequence: " + sequence)
    println("Quality: " + quality)
  }

  def getBarcode: String={
    this.name.split(":").toList.reverse.head
  }

}

/**
 * @object FastqTools
 * @function parseRadSE
 * 	@input file: String	
 * 	@input start: Int
 * 	@input end: Int
 * 	@returns trimmed Fastq instance
 * @function trimIndex
 * 	@input start: Int
 *	@input end: Int
 *	@input string: String
 *	@returns trimmed string
 */

object FastqTools {

  type OptionMap = Map[Symbol, Any]

  def parseRadSE(opt: OptionMap): Iterator[Fastq]={
    // Create IO
    val ifile = new File(opt('infile).toString)
    val ofile = new File(opt('outfile).toString)
    val ostream = new java.io.PrintWriter(ofile)

    // Process file and create iterator of Fastq records
    try {
      val gzip = new GZIPInputStream(new FileInputStream(ifile))
      for {rec <- scala.io.Source.createBufferedSource(gzip).getLines() grouped 4}
        if (opt.isDefinedAt('start) && opt.isDefinedAt('end))
          yield new Fastq(rec(0), 
                          trimIndex(opt('start).asInstanceOf[Int], 
                                    opt('end).asInstanceOf[Int], 
                                    rec(3)),
                          opt.('qv_offset).asInstanceOf[Int]))
      gzip.close()
    } catch {
        case notzip: java.util.zip.ZipException =>
          for {ref <- scala.io.Source.fromFile(ifile).getLines() grouped 4}
            yield new Fastq(rec(0),
                            trimIndex(opt('start).asInstanceOf[Int], 
                                      opt('end).asInstanceOf[Int], 
                                      rec(3)),
                            opt.('qv_offset).asInstanceOf[Int]))
    } finally ostream.close()
  } 

  def trimIndex(start: Int, end: Int, string: String): String={
    string.slice(start-1, end)
  }

}
