name := "ScalaSCA"

version := "0.1"

scalaVersion := "2.11.0-RC1"

mainClass in (Compile, run) := Some("scalasca.core.ScalaSCA")

libraryDependencies ++= Seq(
	"org.scala-lang" % "scala-library-all" % "2.11.0-RC1",
	"org.scala-lang" % "scala-reflect" % "2.11.0-RC1",
	"org.scala-lang" % "scala-compiler" % "2.11.0-RC1",
	"junit" % "junit" % "4.11" % "test",
	"org.hamcrest" % "hamcrest-core" % "1.3",
	"com.novocode" % "junit-interface" % "0.10" % "test",
	"org.scala-sbt" % "test-interface" % "1.0"
)

lazy val buildSettings = Seq(
	version := "0.1-SNAPSHOT",
	scalaVersion := "2.11.0-RC1"
)

mappings in (Compile, packageBin) <+= baseDirectory map { base =>
	(base / "src" / "scalac-plugin.xml") -> "scalac-plugin.xml"
}

fork := true

