/**
 * @author Kyle Hernandez
 *
 */

package com.ngs.cmdline
import scala.compat.Platform.currentTime

trait CommandLineApp {

  val executionStart: Long = currentTime
  protected def args: Array[String] = _args
  private var _args: Array[String] = _
  
  def mainInstance(args: Array[String]) = {
    val commandParser = new CommandLineParser
    this._args = args
    println(args.mkString)
  }
}

