val chiselVersion_r = System.getProperty("chiselVersion", "2.3-SNAPSHOT")

organization := "edu.berkeley.eecs"

version := "0.1-SNAPSHOT"

name := "pktfilter"

scalaVersion := "2.10.3"

libraryDependencies += "edu.berkeley.cs" %% "chisel" % chiselVersion_r

libraryDependencies += "edu.berkeley.eecs" %% "kvstore" % "0.1-SNAPSHOT"
