name := "ScalaSCA"

organization := "lara.epfl"

autoCompilerPlugins := true

version := "0.1-SNAPSHOT"

scalaVersion := "2.11.1"

mainClass in (Compile, run) := Some("scalasca.core.ScalaSCA")

libraryDependencies ++= Seq(
	"org.scala-lang" % "scala-library-all" % "2.11.0",
	"org.scala-lang" % "scala-reflect" % "2.11.0",
	"org.scala-lang" % "scala-compiler" % "2.11.0",
	"junit" % "junit" % "4.11" % "test",
	"org.hamcrest" % "hamcrest-core" % "1.3",
	"com.novocode" % "junit-interface" % "0.10" % "test",
	"org.scala-sbt" % "test-interface" % "1.0"
)

//addCompilerPlugin("lara.epfl" %% "scalasca" % "0.1-SNAPSHOT")

scalacOptions ++= Seq("-unchecked", "-deprecation", "-Xplugin:../ScalaSCA/scalasca_2.11-0.1-SNAPSHOT.jar")

mappings in (Compile, packageBin) <+= baseDirectory map { base =>
	(base / "src" / "scalac-plugin.xml") -> "scalac-plugin.xml"
}

fork := true

