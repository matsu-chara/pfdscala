name := "pfdscala"

version := "0.1"

scalaVersion := "2.13.0"

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.0.8" % "test",
  "org.typelevel" %% "cats-core" % "2.0.0-M4"
)

scalacOptions ++= Seq("-deprecation", "-feature", "-language:higherKinds")