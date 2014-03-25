package scalasca.tests.unit.rules

import org.junit._
import scala.tools.nsc.reporters.ConsoleReporter
import scala.tools.nsc.Global
import scala.tools.nsc.Settings

class BasicTest {

    val settings = new Settings
    settings.verbose.value = true
    settings.Ylogcp.value  = true
	val global = new Global(new Settings, new ConsoleReporter(settings))
	val run = new global.Run
	import global._

	def testTreeEquality(computedTree: Tree, expectedTree: Tree): Unit = {
		assert(computedTree.canEqual(expectedTree), "tests")
	}
}