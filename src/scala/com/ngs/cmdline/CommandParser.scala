package com.ngs.cmdline
import collection.mutable.ListBuffer

class CommandParser(
    programName: String,
    version: String) {
  private val commands = new ListBuffer[Command]

  def split(s: String): (String, String) = s.indexOf('=') match {
    case -1 => throw new IllegalArgumentException("Expected a key=value pair")
    case n: Int => (s.slice(0, n), s.slice(n + 1, s.length))
  }

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

  def help(short: Opetion[String], name: String, description: String) = 
    add(new Help(short, name, description, {this.showUsage; sys.exit}))

  private def descriptions: Seq[String] = commands.map(cmd => cmd match {
    case x if x.isHelp =>
      (x.short { o => "-" + o + " | " + } getOrElse { "" }) + 
      "--" + x.name + " help" + "\t" + x.description
    case _ =>
      (x.name map { o => x.keyName + "=" + x.valueName + " | " } getOrElse { "" } ) +
      "\t" + x.description
  })

  def usage: String = {
    val appText = programName map { _ + " " } getOrElse { "" }
    val versionText = programName map { prog => 
      version map { "\t" + pg | " " + _ } getOrElse { "" }
    } getOrElse { "" }
}


