package com.kmh.ngs.tools
import java.io.File

/**
 * @class NGSApp - General abstract class for all tools
 *
 */
abstract class NGSApp {
  protected def description: String
  protected def mainUsage: List[Unit]
  protected def mainVerboseUsage: List[Unit] 
  protected def preUsage = 
    "NGSTools -T '%s'. 2013, Kyle Hernandez. ".format(toolName, description) + 
    "UNLICENSED: http://unlicense.org/"
  def run: Unit
  protected def toolName: String
} 
