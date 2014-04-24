package lara.epfl.scalasca.tests.unit.rules

import lara.epfl.scalasca.core._
import lara.epfl.scalasca.rules._
import scala.reflect.runtime.universe._
import org.junit._

trait DivisionByZeroTest extends BasicTest {

	def runDBZTest(test: String) = super.runTest("divisionbyzero", test)

	@Test def DivisionByZeroDefaultTest: Unit = runDBZTest("DivisionByZeroDefaultTest")
}