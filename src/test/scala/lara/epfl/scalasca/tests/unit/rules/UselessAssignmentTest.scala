package lara.epfl.scalasca.tests.unit.rules

import lara.epfl.scalasca.core._
import lara.epfl.scalasca.rules._
import scala.reflect.runtime.universe._
import org.junit._

trait UselessAssignmentTest extends BasicTest {

	def runUATest(test: String) = super.runTest("uselessassignment", test)

	@Test def UAFlatTest: Unit = runUATest("UselessAssignmentFlatTest")
	@Test def UANestedTest: Unit = runUATest("UselessAssignmentNestedTest")

	@Test def UAReg1Test: Unit = runUATest("UselessAssignmentReg1Test")
}