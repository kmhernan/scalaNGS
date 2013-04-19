package com.ngs.cmdline

/**
 * Represents Comands for a CommandLineProgram
 * 
 * @author Kyle Hernandez
 */
private case class Command(
    short: Option[String],
    name: String,
    keyName: String,
    valueName: String,
    description: String,
    action: String => Unit,
    getNextArgument: Boolean,
    isHelp: Boolean) {
  def shortDescription = "option " + name 
}

private object KeyValueParser {
  def split(s: String): (String, String) = s.indexOf('=') match {
    case -1 => throw new IllegalArgumentException("Expected a key=value pair")
    case n: Int => (s.slice(0,n), s.slice(n + 1, s.length))
  }
}

/**
 * These classes represent the various types of KeyValue commands and help
 *
 */
private class Help(
    short: Option[String],
    name: String,
    description: String,
    action: => Unit
    ) extends Command(short, name, null, null, description,
                     { a: String => action }, false, true)
 
private class KeyValueCommand(
    name: String,
    keyName: String,
    valueName: String,
    description: String,
    action: (String, String) => Unit
    ) extends Command(null, name, keyName, valueName, description,
                     { a: String => action(KeyValueParser.split(a)._1, KeyValueParser.split(a)._2) },
                     false, false)

private class KeyIntValueCommand(
    name: String,
    keyName: String,
    valueName: String,
    description: String,
    action: (String, Int) => Unit
    ) extends Command(null, name, keyName, valueName, description,
                      { a: String => action(KeyValueParser.split(a)._1, (KeyValueParser.split(a)_2).toInt) },
         	      false, false)

private class KeyDoubleValueCommand(
    name: String,
    keyName: String,
    valueName: String,
    description: String,
    action: (String, Double) => Unit
    ) extends Command(null, name, keyName, valueName, description,
                      { a: String => action(KeyValueParser.split(a)._1, (KeyValueParser.split(a)_2).toDouble) },
         	      false, false)

private class KeyBooleanValueCommand(
    name: String,
    keyName: String,
    valueName: String,
    description: String,
    action: (String, Boolean) => Unit
    ) extends Command(null, name, keyName, valueName, description,
                      { a: String => 
                           val x = KeyValueParser.split(a)
                           val key = x._1
                           val boolVal = x._2.toLowerCase match {
                             case "true" => true
                             case "false" => false
                             case _ => throw new IllegalArgumentException("Expected boolean")
                           }
                           action(key, boolVal)
                      }, false, false)


