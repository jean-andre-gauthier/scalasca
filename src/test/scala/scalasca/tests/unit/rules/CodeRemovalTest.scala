package scalasca.tests.unit.rules

import scalasca.core._
import scalasca.rules._
import scala.reflect.runtime.universe._
import org.junit._

trait CodeRemovalTest extends BasicTest {

	import global._

	@Test def ifTrueTest: Unit = {
		val input = q"""class A { def m(): Unit = { if (true) println("true") else println("false") }}"""
		val expectedOutput = q"""class A { def m(): Unit = { println("true") }}"""
		val cleanedTree = (new UnusedCodeRemoval[global.type]()(global)).apply(input, List())
		testTreeEquality(cleanedTree.tree, expectedOutput)
	}

	@Test def ifFalseTest: Unit = {
		val input = q"""class A { def m(): Unit = { if (false) println("true") else println("false") }}"""
		val expectedOutput = q"""class A { def m(): Unit = { println("false") }}"""
		val cleanedTree = (new UnusedCodeRemoval[global.type]()(global)).apply(input, List())
		testTreeEquality(cleanedTree.tree, expectedOutput)
	}
}