package com.kmh.ngs.tools
import java.io.File
import com.kmh.ngs.cmdline.Arguments
import com.kmh.ngs.io.IoUtil

/**
 * @class NGSApp - General abstract class for all tools
 *
 */
trait NGSApp {
  type OptionMap = Map[Symbol, Any]
  val ioInit = new IoUtil
  def description: String 
  def toolName: String
  def preUsage: String = 
    "NGSTools -T '%s'. 2013, Kyle Hernandez. '%s' ".format(toolName, description) + 
    "UNLICENSED: http://unlicense.org/"
  def mainUsage: Unit
  def mainVerboseUsage: Unit
  def run: Unit
} 
