/**
 * Kyle Hernandez
 * kmhernan@utexas.edu
 * 
 * SEQtools
 */
import scala.util.matching.Regex
import scala.collection.mutable
import java.io._
import java.util.zip.GZIPInputStream

object SEQtools {

  type OptionMap = Map[Symbol, Any]
  val Pattern = """(A+A)$""".r
  var ct_map = scala.collection.mutable.Map[String, Int]("Total"->0, "TooShort"->0, "LowQ"->0, "PolyA"->0, "Passed"->0)

  def main(args: Array[String]): Unit={
    val start = System.currentTimeMillis()
    // Parse arguments 
    if (args.length == 0) mainUsage
    val arglist = args.toList
    def nextOption(map: OptionMap, list: List[String]): OptionMap = {
      list match {
        case Nil => map
        case "-i" :: value :: tail => nextOption(map ++ Map('infile -> value), tail)
        case "-o" :: value :: tail => nextOption(map ++ Map('outfile -> value), tail) 
        case "--start" :: value :: tail => nextOption(map ++ Map('start -> value.toInt), tail)
        case "--qual" :: value :: tail => nextOption(map ++ Map('qual -> value.toInt), tail)
        case "--hpoly" :: value :: tail => nextOption(map ++ Map('hpoly -> value.toDouble), tail)
        case option :: tail => println("Unknown option "+option); mainUsage; sys.exit(1)
      }
    }

    // Create IO 
    val options = nextOption(Map(),arglist)
    val ifile   = new File(options('infile).toString)
    val ofile   = new File(options('outfile).toString)
    val ostream = new java.io.PrintWriter(ofile)

    // Process reads
    try { 
      val gzip = new GZIPInputStream(new FileInputStream(ifile))
      for {rec <- scala.io.Source.createBufferedSource(gzip).getLines() grouped 4}
        process_record(rec.toList, ostream, options)
      gzip.close()
    } catch {
        case notzip: java.util.zip.ZipException => 
          for {rec <- scala.io.Source.fromFile(ifile).getLines() grouped 4}
            process_record(rec.toList, ostream, options)
    } finally ostream.close()

    // Clean up and print log
    ostream.close()
    println("File="+ifile.getName()+" Total="+ct_map("Total")+" PolyA="+ct_map("PolyA")
            +" TooShort="+ct_map("TooShort")+" LowQ="+ct_map("LowQ")+" Passed="+ct_map("Passed")
            +" Ratio="+"%.2f".format(ct_map("Passed")/ct_map("Total").toFloat)
            +" Time="+((System.currentTimeMillis() - start) / 1000.00)+"s")
  }

  def process_record(rec: List[String],
                     o: java.io.PrintWriter, 
                     opt: OptionMap): Unit={
    ct_map("Total") += 1
    val readid = rec(0)
    var sequence = rec(1)
    var quality = rec(3)
      
    sequence = sequence.slice(opt('start).asInstanceOf[Int], sequence.length)
    quality = quality.slice(opt('start).asInstanceOf[Int], quality.length)
    if (isPolyA(sequence, opt('hpoly).asInstanceOf[Double])) {
      ct_map("PolyA") += 1
      val findPoly = (Pattern split sequence).toList
      findPoly match {
        case Nil => sequence = null 
        case string => sequence = string(0)
      }

      if (sequence != null && sequence.length > 30) {
        if (isHighQuality(quality, opt('qual).asInstanceOf[Int])) {
          ct_map("Passed") += 1
          writeRecord(readid, sequence, quality.slice(0, sequence.length), o)
        } else 
            ct_map("LowQ") += 1
      } else 
          ct_map("TooShort") += 1 
    }

    else {
      if (isHighQuality(quality, opt('qual).asInstanceOf[Int])) {
        ct_map("Passed") += 1
        writeRecord(readid, sequence, quality, o)
      }
      else {
        ct_map("LowQ") += 1
      }
    }
  }

  def writeRecord(id: String, seq: String, qual: String, o: java.io.PrintWriter): Unit={
    o.println(id)
    o.println(seq)
    o.println('+')
    o.println(qual)
  }

  def isPolyA(sequence: String, hlimit: Double): Boolean = {
    val string = "A"*(sequence.length * hlimit).toInt
    if (sequence.endsWith(string)) 
      true
    else
      false
  }

  def isHighQuality(quality: String, qlimit: Int): Boolean = {
    val qualArr: Array[Int] = quality.map(_.toInt - 33).toArray
    if ((qualArr.sum.toFloat / qualArr.length) < qlimit)
      false
    else
      true
  }
    
  def mainUsage(): Unit={
    println("usage: scala SEQtools -i file.fastq -o file.fastq --start [int] --hpoly [Double] --qual [Int]")
    sys.exit(1)
  }

}
