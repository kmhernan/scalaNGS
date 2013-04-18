package com.ngs.cmdline

class CommandLineParser {
  val TRUE_FALSE_VALUES = Array(true, false)
  val defaultUsage = "Usage: program [options]"
  val SKELETON_OPTION_DOC = Array(Array("--help", "-h", "Display options for this tool."),
                                  Array("--version", null, "Displays program version."))
  
}
