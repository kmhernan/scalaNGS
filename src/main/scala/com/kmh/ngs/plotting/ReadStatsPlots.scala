package com.kmh.ngs.plotting
import scala.sys.process._

class QualityBoxWhiskersPlot {
  val cmdPrefix = "-e "
  val cmdContainer = new ListBuffer[String]
}

object QualityBoxWhiskersPlots {
  def apply = {
    val qPlot = new QualityBoxWhiskersPlot
    qPlot.add()
  }

} 
