package com.kmh.ngs.tools
import com.kmh.ngs.io._
import java.io.File

/**
 * @class NGSApp - General abstract class for all tools
 *
 */
abstract class NGSApp {
  type OptionMap = Map[String, Any]
  protected val ioInstance = new IoUtil

  def anyToFile(a: Any) = a.asInstanceOf[File]
  def anyToString(a: Any) = a.asInstanceOf[String]
  def anyToDbl(a: Any) = a.asInstanceOf[Double]
  def anyToInt(a: Any) = a.asInstanceOf[Int]

  protected def description: String
  protected def mainUsage: String
  protected def mainVerboseUsage: String
  protected def preUsage = 
    "NGSTools -T '%s'. 2013, Kyle Hernandez. ".format(toolName, description) + 
    "UNLICENSED: http://unlicense.org/"
  def run: Unit
  protected def toolName: String
} 
