/**
 * Kyle Hernandez
 * OptionParser - package to parse options for RADtools and SEQtools
 *
 */

package com.lib.argparse

abstract class AttributeHolder {
  def repr:

class OptionParser {
  private var options = Map[String, Argument]

  def add_argument(name: String, value: Any, help: String): Unit = {
    options ++ Map(name, Argument(name, value, help))
  }

  def print: Unit = {
    println(options)
  }
} 

class Argument(name: String, value: Any, help: String) {
   
} 
