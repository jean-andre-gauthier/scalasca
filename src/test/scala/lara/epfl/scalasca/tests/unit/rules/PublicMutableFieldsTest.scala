package lara.epfl.scalasca.tests.unit.rules

import lara.epfl.scalasca.core._
import lara.epfl.scalasca.rules._
import scala.reflect.runtime.universe._
import org.junit._

trait PublicMutableFieldsTest extends BasicTest {

	def runPMFTest(test: String) = super.runTest("publicmutablefields", test)

	@Test def PublicMutableFieldsClassTest: Unit = runPMFTest("PublicMutableFieldsClassTest")
	@Test def PublicMutableFieldsObjectTest: Unit = runPMFTest("PublicMutableFieldsObjectTest")
	@Test def PublicMutableFieldsTraitTest: Unit = runPMFTest("PublicMutableFieldsTraitTest")
}