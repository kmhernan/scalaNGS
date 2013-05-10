import scala.io.Source
import scala.sys.process._
import java.io.{File, IOException}

object GNUplot {

  def main(args: Array[String]): Unit = {
   val data = scala.io.Source.fromFile(args(0)).getLines.toList.mkString("\n")
   
   def chk = 
     Seq("echo", data) #| Seq(
	"uplot",
        "-e", "reset",
	"-e", "set term png size 2048,768",
        "-e", "set grid",
        "-e", "plot '-' using 1:7:3:4:9 with candlesticks lt 1 " +
	"lw 1 title 'Quartiles' whiskerbars;") #> new File("test.png")
  }
}
