
package test {
  import com.scalangs.io.IoUtil
  import com.scalangs.fastq._
  import java.io.File
  object Test {
    def main(args: Array[String]): Unit = {
      val file = new File(args(0))
      for (rec <- com.scalangs.fastq.FastqReader.parseFastq(file))
        println(rec.getQuality())
    }

  }

}
