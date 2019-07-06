name := "pfdscala"

version := "0.1"

scalaVersion := "2.13.0"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.8" % "test"

scalacOptions ++= Seq("-deprecation", "-feature", "-language:higherKinds")