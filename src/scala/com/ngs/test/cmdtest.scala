
//package test {
import com.ngs.cmdline._

object Test {
  var INPUT = ""
  val appControl = new CommandParser("TestCmd", "1.0.1") {
    newKeyValue("input", "Input file", "INPUT", "filename",
      {(key: String, value: String) => {INPUT=value} })
  }

  def main(args: Array[String]): Unit = {
    appControl.parse(args)   
  }

}

//}
