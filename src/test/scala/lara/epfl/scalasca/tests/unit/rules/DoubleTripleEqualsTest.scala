package lara.epfl.scalasca.tests.unit.rules

import lara.epfl.scalasca.core._
import lara.epfl.scalasca.rules._
import scala.reflect.runtime.universe._
import org.junit._

trait DoubleTripleEqualsTest extends BasicTest {

	def runDTETest(test: String) = super.runTest("doubletripleequals", test)

	@Test def DoubleTripleEqualsActorTest: Unit = runDTETest("DoubleTripleEqualsActorTest")
}