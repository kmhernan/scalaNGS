/** 
 * Kyle Hernandez
 * kmhernan@utexas.edu
 * Package: com.read.Read - contains classes, traits, modules, for NGS reads
 *
 */

package com.read.seq

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

class Fastq(val name: String, val sequence: String, 
            val opt: String, val quality: String,
            val offset: Int) extends Read {

  def averageQuality: Float={
    val test: Array[Int] = this.quality.map(_.toInt - this.offset).toArray
    return test.sum.toFloat / test.length
  }

  def _ns(p: Char): Boolean = { p == 'N' }

  def isNotMissing(good: Double): Boolean={
    if (this.sequence.count(_ns).toFloat / this.sequence.length < good)
      true
    else
      false
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

  def parseRadSE(file: String, start: Int, end: Int, offset: Int): Iterator[Fastq]={
    val fh = scala.io.Source.fromFile(file).getLines()
    val git = fh grouped 4
    for {rec <- git}
      yield new Fastq(rec(0),
                      trimIndex(start, end, rec(1)),
                      rec(2), trimIndex(start, end,rec(3)),
                      offset)
  } 

  def trimIndex(start: Int, end: Int, string: String): String={
    string.slice(start-1, end)
  }

}
