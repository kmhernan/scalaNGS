package com.ngs.cmdline
import collection.mutable.ListBuffer

class CommandParser(
    programName: String,
    version: String) {
  private val commands = new ListBuffer[Command]
  private val NL = System.getProperty("line.separator")

  private def add(command: Command) {
    commands += command
  }

  def newKeyValue(name: String, description: String, keyName: String, valueName: String,
      action: (String, String) => Unit) = 
    add(new KeyValueCommand(name, keyName, valueName, description, action))
  
  def newKeyIntValue(name: String, description: String, keyName: String, valueName: String,
      action: (String, Int) => Unit) = 
    add(new KeyIntValueCommand(name, keyName, valueName, description, action))

  def newKeyDoubleValue(name: String, description: String, keyName: String, valueName: String,
      action: (String, Double) => Unit) = 
    add(new KeyDoubleValueCommand(name, keyName, valueName, description, action))

  def newKeyBooleanValue(name: String, description: String, keyName: String, valueName: String,
      action: (String, Boolean) => Unit) = 
    add(new KeyBooleanValueCommand(name, keyName, valueName, description, action))

  def help(short: Option[String], name: String, description: String) = 
    add(new Help(short, name, description, {this.printUsage; sys.exit}))

  /*private def descriptions: Seq[String] = commands.map(cmd => cmd match {
    case x if x.isHelp =>
      (x.short map { o => "-" + o + " | "} getOrElse { "" }) +
      "--" + x.name + " help" + NL + "\t" + x.description
    case _ =>
      (x.name map { o => x.keyName + "=" + x.valueName + " | " } ) +
      NL + "\t" + x.description
  })*/

  def usage: String = {
    val appText = programName map { _ + " " }
    val versionText = programName map { prog => 
      version map { "\t" + prog + " | " + _ }
    }
    val commandText = if (commands.isEmpty) {""} else {"[arguments] "}
    versionText + NL + "Usage: " + programName + commandText + NL + NL
    //" " + descriptions.mkString(NL + " ") + NL
  }

  def printUsage = Console.err.println(usage) 

  /**
   * Parses the commands
   *
   */
  def parse(args: Seq[String]): Boolean = {
    println(commands)
    args.foreach(a => println(a)) 
    true
  } 
}


