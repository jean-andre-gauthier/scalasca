package lara.epfl.scalasca.tests.unit.rules

import lara.epfl.scalasca.core._
import lara.epfl.scalasca.rules._
import scala.reflect.runtime.universe._
import org.junit._

trait UnfreedResourcesTest extends BasicTest {

	def runURTest(test: String) = super.runTest("unfreedresources", test)

//	@Test def UnfreedResourcesIfTest: Unit = runURTest("UnfreedResourcesIfTest")
	@Test def UnfreedResourcesWhileTest: Unit = runURTest("UnfreedResourcesWhileTest")
//	@Test def UnfreedResourcesMatchTest: Unit = runURTest("UnfreedResourcesMatchTest")
//	@Test def UnfreedResourcesTryTest: Unit = runURTest("UnfreedResourcesTryTest")
}