package lara.epfl.scalasca.tests.unit.rules

import lara.epfl.scalasca.core._
import lara.epfl.scalasca.rules._
import scala.reflect.runtime.universe._
import org.junit._

trait UnusedCodeRemovalTest extends BasicTest {

	def runUCRTest(test: String) = super.runTest("unusedcoderemoval", test)

	@Test def UCRFieldIfTest: Unit = runUCRTest("UnusedCodeRemovalFieldIfTest")
	@Test def UCRIfElseIfTest: Unit = runUCRTest("UnusedCodeRemovalIfElseIfTest")
	@Test def UCRIfFalseTest: Unit = runUCRTest("UnusedCodeRemovalIfFalseTest")
	@Test def UCRIfTrueTest: Unit = runUCRTest("UnusedCodeRemovalIfTrueTest")
//	@Test def UCRWhileTest: Unit = runUCRTest("UnusedCodeRemovalWhileTest")
//	@Test def UCRDoWhileTest: Unit = runUCRTest("UnusedCodeRemovalDoWhileTest")
}