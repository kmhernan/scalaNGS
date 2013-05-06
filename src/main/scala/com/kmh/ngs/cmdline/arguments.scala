package com.kmh.ngs.cmdline
import java.io.File

trait Arguments {
  type OptionMap = Map[Symbol, Any]
  val required: List[Symbol]
  def mainUsage: Unit 
  def mainVerboseUsage: Unit 
  def parse(map: OptionMap, list: List[String]): OptionMap
  def checkRequired(map: OptionMap): OptionMap
} 
