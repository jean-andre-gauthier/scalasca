package scalasca.tests.unit.rules

import scalasca.core._
import scalasca.rules._
import scala.reflect.runtime.universe._
import org.junit._

trait BlockConstantPropagationTest extends BasicTest {

	import global._

	@Test def integerAddTest: Unit = {
		val toTest = Apply(Ident(newTermName("println")), List(Literal(Constant(2))))
//		val toTest = q"println(2)" //Why does not work?
		val propagatedTree = (new BlockConstantPropagation[global.type]()(global)).apply(toTest, List())
		testTreeEquality(propagatedTree.tree, toTest)
	}
}