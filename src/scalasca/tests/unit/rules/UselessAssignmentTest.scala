package scalasca.tests.unit.rules

import scalasca.core._
import scalasca.rules._
import scala.reflect.runtime.universe._
import org.junit._

trait UselessAssignmentTest extends BasicTest {

	import global._

	@Test def ifBlockRemovalTest: Unit = {
		val input = q"""class A { def m: Integer = { val a = 12; if (a < 13) println("true") else println("false") } }"""
		val expectedOutput = ""
		val useless = (new UselessAssignment[global.type]()(global)).apply(input, List())
		assert(useless.toTestString == expectedOutput)
	}
}