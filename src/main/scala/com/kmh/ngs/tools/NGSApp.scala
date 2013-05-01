package com.kmh.ngs.tools
import java.io.File

/**
 * @class NGSApp - General abstract class for all tools
 *
 */
abstract class NGSApp {
  protected def description: String
  protected def mainUsage: String
  protected def mainVerboseUsage: String
  protected def preUsage = 
    "NGSTools -T '%s'. 2013, Kyle Hernandez. ".format(toolName, description) + 
    "UNLICENSED: http://unlicense.org/"
  def run: Unit
  protected def toolName: String
} 
