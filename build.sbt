scalaVersion := "2.12.3"

name := "hello-cats"

organization := "com.github.arturopala"

version := "1.0"

libraryDependencies := Seq(
  "org.typelevel" %% "cats-core" % "1.0.0-MF" ,
  "org.typelevel" %% "cats-laws" % "1.0.0-MF" ,
  "org.scalacheck" %% "scalacheck" % "1.13.4" % "test",
  "org.scalatest" %% "scalatest" % "3.0.1" % "test",
  "org.typelevel" %% "discipline" % "0.7.3"
)