package com.kmh.ngs.tools
import java.io.File
import com.kmh.ngs.cmdline.Arguments
import com.kmh.ngs.IoUtil

/**
 * @class NGSApp - General abstract class for all tools
 *
 */
abstract class NGSApp extends Arguments {
  protected val ioInit = new IoUtil
  protected def description: String
  protected def toolName: String
  protected def preUsage = 
    "NGSTools -T '%s'. 2013, Kyle Hernandez. ".format(toolName, description) + 
    "UNLICENSED: http://unlicense.org/"
  def run: Unit
} 
