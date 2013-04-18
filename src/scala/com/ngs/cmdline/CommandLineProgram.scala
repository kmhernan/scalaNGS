package com.ngs.cmdline

abstract class CommandLineProgram {
  
 private val commandLineParser: CommandLineParser
 private val commandLine: String
 protected def doWork: Int

