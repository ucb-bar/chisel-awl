import scala.sys.process._
// OBS: sbt._ has also process. Importing scala.sys.process 
// and explicitly using it ensures the correct operation

organization := "edu.berkeley.cs"

name := "hbwif"

version := scala.sys.process.Process("git rev-parse --short HEAD").!!.mkString.replaceAll("\\s", "")+"-SNAPSHOT"


scalaVersion := "2.12.4"

