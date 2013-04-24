import AssemblyKeys._

assemblySettings

jarName in assembly += "NGSTools.jar"

//mainClass in assembly := com.kmh.ngs.NGSTools

// --------------------------------------------------------
// Basic settings

name := "NGSTools"

organization := "com.kmh"

version := "1.0.0"

licenses := Seq("UNLICENSE" -> url("http://unlicense.org/UNLICENSE"))

homepage := Some(url("https://github.com/kmhernan/scalaNGS"))

description := "A full suite of NGS tools"

scalaVersion := "2.10.0"

//mainClass := com.kmh.ngs.NGSTools

// Dependencies

//libraryDependencies += "org.clapper" % "grizzled-scala_2.10" % "1.1.3" 
