package com.kmh.ngs.statistics
import com.kmh.ngs.readers.ReadReader
object qualMatrix {
  def apply(rr: ReadReader, offset: Int): Unit = {
    val myArray = rr.iter.map(_.quality.map(_.toInt - offset).toArray).toArray
    val myIter = myArray.iterator.map(_.iterator)
    myIter.foreach(x => x.foreach(y => {
      var println(y))) 
   }
} 
