/**
 * Kyle Hernandez
 * kmhernan@utexas.edu
 * 
 * RADtools - Various tools for handling RAD NGS sequences.
 */
import com.read._

object RADtools extends App {

  for (record <- FastqParser.parseSE(args(0)))
    record.repr
}
