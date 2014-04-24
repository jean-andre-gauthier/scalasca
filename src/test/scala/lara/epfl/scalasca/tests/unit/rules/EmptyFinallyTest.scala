package lara.epfl.scalasca.tests.unit.rules

import lara.epfl.scalasca.core._
import lara.epfl.scalasca.rules._
import scala.reflect.runtime.universe._
import org.junit._

trait EmptyFinallyTest extends BasicTest {

	def runEFTest(test: String) = super.runTest("emptyfinally", test)

	@Test def EmptyFinallyDefaultTest: Unit = runEFTest("EmptyFinallyDefaultTest")
}