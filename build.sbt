import AssemblyKeys._

assemblySettings

// Basic settings

name := "NGSTools"

organization := "com.kmh"

version := "1.0.0"

licenses := Seq("UNLICENSE" -> url("http://unlicense.org/UNLICENSE"))

homepage := Some(url("https://github.com/kmhernan/scalaNGS"))

description := "A full suite of NGS tools"

scalaVersion := "2.10.0"

// Dependencies

libraryDependencies ++= Seq(
  "org.eintr.loglady" %% "loglady" % "1.1.0",
  "ch.qos.logback" % "logback-classic" % "0.9.29")

//libraryDependencies += "org.clapper" % "grizzled-scala_2.10" % "1.1.3" 

//libraryDependencies += "com.dongxiguo" %% "zero-log" % "0.3.3" 
