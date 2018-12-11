import sys.process._
organization := "edu.berkeley.cs"

name := "hbwif"

version := ("git rev-parse --short HEAD"!!).mkString.replaceAll("\\s", "")+"-SNAPSHOT"

scalaVersion := "2.11.11"

libraryDependencies += "berkeley" %% "rocketchip" % "1.2"

