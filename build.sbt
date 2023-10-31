ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.12"

lazy val root = (project in file("."))
  .settings(
    name := "LineBreaking"
  )

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest-wordspec" % "3.2.17" % "test"
)