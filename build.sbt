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

scalacOptions += "-deprecation"

// Dependencies

libraryDependencies ++= Seq(
  "org.eintr.loglady" %% "loglady" % "1.1.0",
  "ch.qos.logback" % "logback-classic" % "0.9.29")
