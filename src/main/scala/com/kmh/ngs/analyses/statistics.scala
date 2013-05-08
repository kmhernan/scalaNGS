package com.kmh.ngs.statistics
import com.kmh.ngs.readers.ReadReader
object qualMatrix {
  def apply(rr: ReadReader, offset: Int): Unit = {
    val bases = Array[Char]('A', 'C', 'G', 'T', 'N')
    //val myArray = rr.iter.map(_.quality.map(_.toInt - offset).toArray).toArray
    //val tst = myArray.transpose.map(_.foldLeft(0)(_+_))
    //tst.foreach(x => println("%2f".format(x/myArray.length.toDouble)))
    val myArray = rr.iter.map(x => (x.quality.map(_.toInt - offset).toArray, x.sequence.toArray)).toArray
    val qArray = myArray.map(x => x._1).toArray.transpose.map(_.foldLeft(0)(_+_))
    val bArray = myArray.map(x => x._2).toArray.transpose.map(_.groupBy(identity).mapValues(_.size))
    println("Position\tAvgQual\tA\tC\tG\tT\tN")
    for (i<- 0 until qArray.length) {
      val ptBase = bases.map(x => {
	if (bArray(i).isDefinedAt(x)) "%2f\t".format(bArray(i)(x)/myArray.length.toDouble) else "0.0"})
      val ptStr = "%s\t%2f\t".format(i+1, qArray(i)/myArray.length.toDouble) + ptBase
      println(ptStr)
    }
  } 
}
