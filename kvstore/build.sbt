organization := "edu.berkeley.eecs"

version := "0.1-SNAPSHOT"

name := "mc-accel"

scalaVersion := "2.10.3"

addSbtPlugin("com.github.scct" % "sbt-scct" % "0.2")

libraryDependencies += "edu.berkeley.cs" %% "chisel" % "2.3-SNAPSHOT"
