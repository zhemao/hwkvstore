organization := "edu.berkeley.cs"

version := "1.0"

name := "jackhammer"

scalaVersion := "2.10.2"

addSbtPlugin("com.github.scct" % "sbt-scct" % "0.2.1")

libraryDependencies += "edu.berkeley.cs" %% "chisel" % "2.3-SNAPSHOT"

libraryDependencies += "com.typesafe.slick" %% "slick" % "2.0.2"

libraryDependencies += "com.novocode" % "junit-interface" % "0.10" % "test"

libraryDependencies += "org.scalatest" %% "scalatest" % "1.9.2" % "test"
