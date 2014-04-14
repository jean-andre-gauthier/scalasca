package lara.epfl.scalasca.tests.unit.rules

import scala.reflect.runtime.universe.Tree
import scala.sys.process._

class BasicTest {

	def outputToStrippedString(cmd: Seq[String]): String = {
		val stdError = new StringBuffer()
		val stdOutput = cmd lines_! ProcessLogger(stdError append _)
		stdOutput.toList.foldLeft("")((acc, item) => acc + item.toString.trim + "\n")
	}

	def runTest(prefix: String, test: String): Unit = {
		val producedOutput = outputToStrippedString(Seq("scalac", "-d", "bin", "-Xplugin:target/scala-2.11.0-RC1/scalasca_2.11.0-RC1-0.1.jar", "-P:scalasca:testRule:" + prefix, "src/test/scala/lara/epfl/scalasca/tests/unit/executables/" + prefix + "/" + test + ".scala"))
		val expectedOutput = outputToStrippedString(Seq("cat", "src/test/scala/lara/epfl/scalasca/tests/unit/executables/" + prefix + "/" + test + ".txt"))

		assert(producedOutput == expectedOutput, "Produced output:\n" + producedOutput + "\nExpected output:\n" + expectedOutput)
	}
}