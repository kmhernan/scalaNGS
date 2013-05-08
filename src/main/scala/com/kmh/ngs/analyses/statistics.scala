package com.kmh.ngs.statistics
import com.kmh.ngs.readers.ReadReader
import com.kmh.ngs.formats.FQRecord
import org.eintr.loglady.Logging

class ReadIndexData(index: Int){
  var min: Int = 0
  var max: Int = 0
  var count: Int = 0
  val quality_container = Array.fill(50)(0)
  val nucleotide_container = Array.fill(5)(0) 
}

object ReadStatsByIndex extends Logging {
  val ReadStatsHeader = Array[String]("position", "N", "min", "max", "sum", "mean", "Q1", "med", "Q3", "IQR",
				      "A_ct", "C_ct", "G_ct", "T_ct", "N_ct").mkString("\t")
  var totalReads: Int = 0
  def apply(rr: ReadReader, offset: Int): Unit = {
    val readArray = rr.iter.map(rec => {
      rec.quality.map(_.toInt - offset).toArray.foreach(
      
      
    /*val myArray = rr.iter.map(x => (x.quality.map(_.toInt - offset).toArray, x.sequence.toArray)).toArray
    val lenArr = myArray.length
    val qArray = myArray.map(x => x._1).toArray.transpose.map(_.foldLeft(0)(_+_)).map(_/lenArr.toDouble)
    val bArray = myArray.map(x => x._2).toArray.transpose.map(_.groupBy(identity).mapValues(_.size))
      .map(x => bases.map(b => x.get(b) match {
        case Some(s) => "%1.2f".format(s / lenArr.toDouble)
        case None => "%1.2f".format(0.0)
        }))
    log.info("Processed %s reads...".format(lenArr))
    println("Pos\tQual\t%A\t%C\t%G\t%T\t%N")
    bArray.view.zipWithIndex foreach 
      {case(value, index) => println("%s\t%1.2f\t%s".format(index+1, qArray(index), value.mkString("\t")))}
    */
  } 
}
