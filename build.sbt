import AssemblyKeys._

assemblySettings

jarName in assembly := "NGSTools.jar"

// Basic settings

name := "NGSTools"

organization := "com.kmh"

version := "1.0.0"

licenses := Seq("UNLICENSE" -> url("http://unlicense.org/UNLICENSE"))

homepage := Some(url("https://github.com/kmhernan/scalaNGS"))

description := "A full suite of NGS tools"

scalaVersion := "2.10.1"

scalacOptions ++= Seq("-deprecation", "-optimize", "-Yinline-warnings")

// Dependencies

libraryDependencies ++= Seq(
  "org.eintr.loglady" %% "loglady" % "1.1.0",
  "ch.qos.logback" % "logback-classic" % "0.9.29")
