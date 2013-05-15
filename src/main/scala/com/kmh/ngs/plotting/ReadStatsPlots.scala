package com.kmh.ngs.plotting
import scala.sys.process._
import scala.io.Source.fromInputStream
import java.io._
import org.eintr.loglady.Logging

object MultiQualBasesPlot extends Logging {
  def apply(data: String, opng: String): Unit = {
    log.info("Creating Quality and Bases Plot...")
    lazy val plot = Seq(
	"gnuplot",
	"-e", "reset", 
	"-e", "set term png",
	"-e", "set output '%s'".format(opng),
 	"-e", "set grid layerdefault linetype 0 linewidth 1.000, linetype 0 linewidth 1.000",
        "-e", "set yrange [ 0.000 : 55.000 ] noreverse nowriteback",
        "-e", "set lmargin 9",
        "-e", "set rmargin 2",
        "-e", "set tmargin 2",
        "-e", "set multiplot layout 2, 1",
        "-e", "set tics scale 0",
        "-e", "set ylabel 'Quality by Index'",
        "-e", "set style fill solid 0.25",
        "-e", "set style data lines",
        "-e", "set key outside top center horizontal nobox",
        "-e", "plot '-' u 1:($6+$7):($6-$7) t '+/- 1SD' w filledcu, " +
              "'-' u 1:6 w lines t 'Mean' lt 1 lc rgb 'red', " +
              "'-' u 1:8 w lines t 'Median' lt 2 lc rgb 'black'",
        "-e", "set style histogram rowstacked",
        "-e", "set style data histograms",
        "-e", "unset grid",
        "-e", "set style fill solid 1.00 border -1",
	"-e", "set xtic (0, 20, 40, 60, 80, 100)",
        "-e", "set key outside top center horizontal nobox",
        "-e", "set tics scale 0",
        "-e", "set yrange [ 0.0000 : 100.000 ] noreverse nowriteback",
        "-e", "set ylabel 'Base Frequency by Index'",
        "-e", "plot '-' using (100.*$9/$2) title 'A' lt rgb '#DD234E', " +
              "'-' using (100.*$10/$2) title 'C' lt rgb '#21689A', "+
              "'-' using (100.*$11/$2) title 'G' lt rgb '#63D222', "+
              "'-' using (100.*$12/$2) title 'T' lt rgb '#506E3E', "+
              "'-' using (100.*$13/$2) title 'N' lt rgb 'black'",
        "-e", "unset multiplot")
    
    val process = (plot #< Seq("echo", data, "\nend\n", data, "\nend\n", data, "\nend\n",
	data, "\nend\n", data, "\nend\n", data, "\nend\n", data, "\nend\n", data, "\nend"))

    try {
      process.run(new ProcessIO(
	in => {in.close},
	out => {out.close},
	err => {println(fromInputStream(err).mkString("")); err.close})).exitValue match {
          case 0 => log.info("Successfully created plot %s".format(opng))
          case err: Int => log.error("Is your output path writable?"); sys.exit(1)
      }
    } catch {
      case err: Throwable => log.error("Is GNUplot installed and in your path??"+err); sys.exit(1)
    }
  }

}
