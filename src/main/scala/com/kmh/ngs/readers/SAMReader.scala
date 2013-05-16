package com.kmh.ngs.readers

import com.kmh.ngs.formats.SAMRecord
import java.io.{File, BufferedReader, IOException}

class SAMReader(
	val samReader: BufferedReader,
	val samFile: File) {

  var lineCT: Int = 0
  var currentLine: String = nextLine 

  def close: Unit = samReader.close()
  
  def hasNext: Boolean = {currentLine != null}

  def next: SAMRecord = {
    if (!hasNext) { close; return null }
    try SAMRecord(currentLine, lineCT)
    finally currentLine = nextLine
  }

  def nextLine: String = {
    lineCT += 1
    samReader.readLine()
  }

  def skipHeader: Unit = {
    while (currentLine.startsWith("@")) {
      currentLine = nextLine
    }
  }

  def iter: Iterator[SAMRecord] = {
    val it = Iterator.continually {this.next}
    for (rec <- it.takeWhile(_ != null)) yield { rec }
  }
}
