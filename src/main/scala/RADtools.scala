/**
 * Kyle Hernandez
 * kmhernan@utexas.edu
 * 
 * RADtools - Various tools for handling RAD NGS sequences.
 */
import com.read._

object RADtools {

  def main(args: Array[String]): Unit={
    for (record <- FastqTools.parseRadSE(args(0), 1, 36, 33)){
      record.repr
      println(record.averageQuality)
      println(record.countN)
    }
  }
}
