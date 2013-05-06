package com.kmh.ngs.cmdline
import java.io.File

trait Arguments {
  type OptionMap = Map[Symbol, Any]
  val required: List[String]
  def mainUsage: String
  def mainVerboseUsage: String
  def getArg[T](x: T) = x
  def parse(map: OptionMap, list: List[String]): OptionMap
  def checkRequired(map: OptionMap): OptionMap
} 
