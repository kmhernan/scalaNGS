import com.kmh.ngs.cmdline._
import com.kmh.ngs.tools._

object NGSTools extends CommandLineApp {
  
  def main(args: Array[String]): Unit = {
    val analysis = getAnalysis(args.toList)
    analysis.run
  }

}
