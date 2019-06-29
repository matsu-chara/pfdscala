name := "pfdscala"

version := "0.1"

scalaVersion := "2.12.8"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.8" % "test"

scalacOptions ++= Seq("-deprecation", "-feature", "-language:higherKinds")