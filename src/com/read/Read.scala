/** 
 * Kyle Hernandez
 * kmhernan@utexas.edu
 * Package: com.read.Read - contains classes, traits, modules, for NGS reads
 *
 */

package com.read
import scala.io.Source

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

class Fastq(val name: String, val sequence: String, val opt: String, val quality: String) extends Read {

  def averageQuality: Float={
    val test: Array[Int] = this.quality.map(_.toInt).toArray
    val avg: Float = test.sum / test.length
     
  }

  def repr: Unit={
    println("Name: " + name)
    println("Sequence: " + sequence)
    println("Quality: " + quality)
  }

}

/**
 * @object FastqParser
 * @input file: String	
 * @input start: Int
 * @input end: Int
 * @returns trimmed Fastq instance
 */

object FastqParser {

  // Parse SE reads
  def parseSE(file: String): Iterator[Fastq]={
    val fh = scala.io.Source.fromFile(file).getLines()
    val git = fh grouped 4
    for {rec <- git}
      yield new Fastq(rec(0), trim(1, 36, rec(1)), rec(2), trim(1,36,rec(3)))
  } 

  // Trim RAD sequences
  def trim(start: Int, end: Int, string: String): String={
    string.slice(start-1, end)
  }

}
