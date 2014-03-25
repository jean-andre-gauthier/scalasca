import AssemblyKeys._

name := "ScalaSCA"

version := "0.1"

scalaVersion := "2.11.0-M8"

libraryDependencies ++= Seq(
	"org.scala-lang" % "scala-library-all" % "2.11.0-M8",
	"org.scala-lang" % "scala-reflect" % "2.11.0-M8",
	"org.scala-lang" % "scala-compiler" % "2.11.0-M8",
	"junit" % "junit" % "4.11" % "test",
	"org.hamcrest" % "hamcrest-core" % "1.3",
	"com.novocode" % "junit-interface" % "0.10" % "test",
	"org.scala-sbt" % "test-interface" % "1.0"
)

lazy val buildSettings = Seq(
  version := "0.1-SNAPSHOT",
  organization := "com.example",
  scalaVersion := "2.11.0-M8"
)

val app = (project in file("app")).
  settings(buildSettings: _*).
  settings(assemblySettings: _*).
  settings(
    // your settings here
  )