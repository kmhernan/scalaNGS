/** 
 * Kyle Hernandez
 * kmhernan@utexas.edu
 * Package: com.read.Read - contains classes, traits, modules, for NGS reads
 *
 */

package com.read.seq

import scala.io.Source
import java.io._
import java.util.zip.GZIPInputStream

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
            val quality: String, val offset: Int) extends Read {

  def averageQuality: Float = {
    val test: Array[Int] = this.quality.map(_.toInt - this.offset).toArray
    return test.sum.toFloat / test.length
  }

  def repr: Unit = {
    println("Name: " + name)
    println("Sequence: " + sequence)
    println("Quality: " + quality)
  }

  def getBarcode: String = {
    this.name.split(":").toList.reverse.head
  }

  def write(o: java.io.PrintWriter): Unit = {
    o.println(this.name)
    o.println(this.sequence)
    o.println('+')
    o.println(this.quality)
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
    val ifile  = new File(opt('infile).toString)
    // Process file and create iterator of Fastq records
    try {
      val gzip = new GZIPInputStream(new FileInputStream(ifile))
      for (rec <- scala.io.Source.createBufferedSource(gzip).getLines() grouped 4) yield {
        toFastq(rec, opt)
      }
    } catch {
        case notgz: java.util.zip.ZipException =>
          for (rec <- scala.io.Source.fromFile(ifile).getLines() grouped 4) yield
            toFastq(rec, opt) 
        case notgzOSX: java.io.IOException =>
          for (rec <- scala.io.Source.fromFile(ifile).getLines() grouped 4) yield
            toFastq(rec, opt) 
    }
  } 

  // To create FASTQ instance
  def toFastq (rec: Seq[String], opt: OptionMap): Fastq = {
      new Fastq(rec(0), trim(rec(1), opt get 'start, opt get 'end),
                trim(rec(3), opt get 'start, opt get 'end),
                opt('qv_offset).asInstanceOf[Int])
  }

  // Trim as requested
  def trim(string: String, start: Option[Any], end: Option[Any]) = (start, end) match {
    case (Some(s), Some(e)) => string.slice(s.asInstanceOf[Int]-1, e.asInstanceOf[Int])
    case (Some(s), None) => string.slice(s.asInstanceOf[Int]-1, string.length)
    case (None, Some(e)) => string.slice(0, e.asInstanceOf[Int]+1)
    case (None, None) => string 
  }

}
