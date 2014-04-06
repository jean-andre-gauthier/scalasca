package lara.epfl.scalasca.tests.unit.rules

import org.junit._
import scala.tools.nsc.reporters.ConsoleReporter
import scala.tools.nsc.Global
import scala.tools.nsc.Settings
import scala.tools.nsc.CompilationUnits
import scala.tools.reflect.ToolBox
import scala.reflect.runtime.universe
import scala.reflect.runtime.universe._

class BasicTest {

	val settings = new Settings
	settings processArgumentString "-usejavacp"
	val global = new Global(settings)
	val run = new global.Run

	val toolBox = runtimeMirror(getClass.getClassLoader).mkToolBox()

	val importer0 = universe.mkImporter(global.rootMirror.universe)
	val importerA = importer0.asInstanceOf[universe.Importer { val from: global.rootMirror.universe.type }]
	val importer1 = global.rootMirror.universe.mkImporter(universe)
	val importerB = importer0.asInstanceOf[global.rootMirror.universe.Importer { val from: universe.type }]

	import global._

	def symbolisedTree(tree: Tree): Tree = {
		val t = importerA.importTree(tree).duplicate
		toolBox.typecheck(t)
		importerB.importTree(t).duplicate
	}

	def testTreeEquality(computedTree: Tree, expectedTree: Tree): Unit = {

		assert(computedTree.equalsStructure(expectedTree), "Test failed")
	}
}