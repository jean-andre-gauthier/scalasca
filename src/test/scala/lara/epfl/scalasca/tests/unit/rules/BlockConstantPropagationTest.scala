package lara.epfl.scalasca.tests.unit.rules

import lara.epfl.scalasca.core._
import lara.epfl.scalasca.rules._
import scala.reflect.runtime.universe._
import org.junit._

trait BlockConstantPropagationTest extends BasicTest {

	import global._

	@Test def integerAddTest: Unit = {
//		val toTest = Apply(Ident(newTermName("println")), List(Literal(Constant(2))))
		val input = q"class A { def m(): Unit = { val a = 11; val b = a + 15 }}" //Why does not work?
		val expectedOutput = q"class A { def m(): Unit = { val a = 10; val b = 20 }}"
		val propagatedTree = (new BlockConstantPropagation[global.type]()(global)).apply(input, List())
		testTreeEquality(propagatedTree.tree, expectedOutput)
	}

	@Test def integerSubTest: Unit = {
		val input = q"class A { def m(): Unit = { val a = 10; val b = a - 15 }}"
		val expectedOutput = q"class A { def m(): Unit = { val a = 10; val b = -5 }}"
		val propagatedTree = (new BlockConstantPropagation[global.type]()(global)).apply(input, List())
		testTreeEquality(propagatedTree.tree, expectedOutput)
	}

	@Test def integerMulTest: Unit = {
		val input = q"class A { def m(): Unit = { val a = 10; val b = a * 15 }}"
		val expectedOutput = q"class A { def m(): Unit = { val a = 10; val b = 150 }}"
		val propagatedTree = (new BlockConstantPropagation[global.type]()(global)).apply(input, List())
		testTreeEquality(propagatedTree.tree, expectedOutput)
	}

	@Test def integerDivTest: Unit = {
		val input = q"class A { def m(): Unit = { val a = 15; val b = a / 10 }}"
		val expectedOutput = q"class A { def m(): Unit = { val a = 15; val b = 1 }}"
		val propagatedTree = (new BlockConstantPropagation[global.type]()(global)).apply(input, List())
		testTreeEquality(propagatedTree.tree, expectedOutput)
	}

	@Test def doubleDivTest: Unit = {
		val input = q"class A { def m(): Unit = { val a = 15; val b = a / 10 }}"
		val expectedOutput = q"class A { def m(): Unit = { val a = 15; val b = 1.5 }}"
		val propagatedTree = (new BlockConstantPropagation[global.type]()(global)).apply(input, List())
		testTreeEquality(propagatedTree.tree, expectedOutput)
	}

	@Test def integerModTest: Unit = {
		val input = q"class A { def m(): Unit = { val a = 12; val b = a % 5 }}"
		val expectedOutput = q"class A { def m(): Unit = { val a = 10; val b = 2 }}"
		val propagatedTree = (new BlockConstantPropagation[global.type]()(global)).apply(input, List())
		testTreeEquality(propagatedTree.tree, expectedOutput)
	}

	@Test def integerLeqTest: Unit = {
		val input = q"class A { def m(): Unit = { val a = 10; val b = a <= 10 }}"
		val expectedOutput = q"class A { def m(): Unit = { val a = 10; val b = true }}"
		val propagatedTree = (new BlockConstantPropagation[global.type]()(global)).apply(input, List())
		testTreeEquality(propagatedTree.tree, expectedOutput)
	}

	@Test def integerLeTest: Unit = {
		val input = q"class A { def m(): Unit = { val a = 10; val b = a < 10 }}"
		val expectedOutput = q"class A { def m(): Unit = { val a = 10; val b = false }}"
		val propagatedTree = (new BlockConstantPropagation[global.type]()(global)).apply(input, List())
		testTreeEquality(propagatedTree.tree, expectedOutput)
	}

	@Test def integerGeqTest: Unit = {
		val input = q"class A { def m(): Unit = { val a = 10; val b = a >= 10 }}"
		val expectedOutput = q"class A { def m(): Unit = { val a = 10; val b = true }}"
		val propagatedTree = (new BlockConstantPropagation[global.type]()(global)).apply(input, List())
		testTreeEquality(propagatedTree.tree, expectedOutput)
	}

	@Test def integerGeTest: Unit = {
		val input = q"class A { def m(): Unit = { val a = 10; val b = a > 10 }}"
		val expectedOutput = q"class A { def m(): Unit = { val a = 10; val b = false }}"
		val propagatedTree = (new BlockConstantPropagation[global.type]()(global)).apply(input, List())
		testTreeEquality(propagatedTree.tree, expectedOutput)
	}


	@Test def integerOrTest: Unit = {
		val input = q"class A { def m(): Unit = { val a = 4; val b = a | 11 }}"
		val expectedOutput = q"class A { def m(): Unit = { val a = 10; val b = 15 }}"
		val propagatedTree = (new BlockConstantPropagation[global.type]()(global)).apply(input, List())
		testTreeEquality(propagatedTree.tree, expectedOutput)
	}

	@Test def integerAndTest: Unit = {
		val input = q"class A { def m(): Unit = { val a = 10; val b = a & 7 }}"
		val expectedOutput = q"class A { def m(): Unit = { val a = 10; val b = 2 }}"
		val propagatedTree = (new BlockConstantPropagation[global.type]()(global)).apply(input, List())
		testTreeEquality(propagatedTree.tree, expectedOutput)
	}

	@Test def integerXorTest: Unit = {
		val input = q"class A { def m(): Unit = { val a = 10; val b = a ^ 15 }}"
		val expectedOutput = q"class A { def m(): Unit = { val a = 10; val b = 10 }}"
		val propagatedTree = (new BlockConstantPropagation[global.type]()(global)).apply(input, List())
		testTreeEquality(propagatedTree.tree, expectedOutput)
	}

	@Test def integerEqTest: Unit = {
		val input = q"class A { def m(): Unit = { val a = 10; val b = a == 15 }}"
		val expectedOutput = q"class A { def m(): Unit = { val a = 10; val b = false }}"
		val propagatedTree = (new BlockConstantPropagation[global.type]()(global)).apply(input, List())
		testTreeEquality(propagatedTree.tree, expectedOutput)
	}

	@Test def integerNeqTest: Unit = {
		val input = q"class A { def m(): Unit = { val a = 10; val b = a != 15 }}"
		val expectedOutput = q"class A { def m(): Unit = { val a = 10; val b = true }}"
		val propagatedTree = (new BlockConstantPropagation[global.type]()(global)).apply(input, List())
		testTreeEquality(propagatedTree.tree, expectedOutput)
	}

	@Test def booleanEqTest: Unit = {
		val input = q"class A { def m(): Unit = { val a = true; val b = a == true }}"
		val expectedOutput = q"class A { def m(): Unit = { val a = true; val b = true }}"
		val propagatedTree = (new BlockConstantPropagation[global.type]()(global)).apply(input, List())
		testTreeEquality(propagatedTree.tree, expectedOutput)
	}

	@Test def booleanNeqTest: Unit = {
		val input = q"class A { def m(): Unit = { val a = true; val b = a != true }}"
		val expectedOutput = q"class A { def m(): Unit = { val a = true; val b = false }}"
		val propagatedTree = (new BlockConstantPropagation[global.type]()(global)).apply(input, List())
		testTreeEquality(propagatedTree.tree, expectedOutput)
	}

	@Test def booleanAndTest: Unit = {
		val input = q"class A { def m(): Unit = { val a = true; val b = a && false }}"
		val expectedOutput = q"class A { def m(): Unit = { val a = true; val b = false }}"
		val propagatedTree = (new BlockConstantPropagation[global.type]()(global)).apply(input, List())
		testTreeEquality(propagatedTree.tree, expectedOutput)
	}

	@Test def booleanOrTest: Unit = {
		val input = q"class A { def m(): Unit = { val a = true; val b = a || false }}"
		val expectedOutput = q"class A { def m(): Unit = { val a = true; val b = false }}"
		val propagatedTree = (new BlockConstantPropagation[global.type]()(global)).apply(input, List())
		testTreeEquality(propagatedTree.tree, expectedOutput)
	}

	@Test def booleanNotTest: Unit = {
		val input = q"class A { def m(): Unit = { val a = true; val b = !a }}"
		val expectedOutput = q"class A { def m(): Unit = { val a = true; val b = false }}"
		val propagatedTree = (new BlockConstantPropagation[global.type]()(global)).apply(input, List())
		testTreeEquality(propagatedTree.tree, expectedOutput)
	}

	@Test def stringAddTest: Unit = {
		val input = q"""class A { def m(): Unit = { val a = "hello"; val b = a + "world" }}"""
		val expectedOutput = q"""class A { def m(): Unit = { val a = "hello"; val b = "helloworld" }}"""
		val propagatedTree = (new BlockConstantPropagation[global.type]()(global)).apply(input, List())
		testTreeEquality(propagatedTree.tree, expectedOutput)
	}

	@Test def stringLenTest: Unit = {
		val input = q"""class A { def m(): Unit = { val a = "hello"; val b = a.length }}"""
		val expectedOutput = q"""class A { def m(): Unit = { val a = "hello"; val b = 5 }}"""
		val propagatedTree = (new BlockConstantPropagation[global.type]()(global)).apply(input, List())
		testTreeEquality(propagatedTree.tree, expectedOutput)
	}

	@Test def classFieldTest: Unit = {
		val input = q"class A { val a = 12; val b = a + 13 }"
		val expectedOutput = q"class A { val a = 12; val b = a + 13 }"
		val propagatedTree = (new BlockConstantPropagation[global.type]()(global)).apply(input, List())
		testTreeEquality(propagatedTree.tree, expectedOutput)
	}

	@Test def objectFieldTest: Unit = {
		val input = q"object A { val a = 12; val b = a + 13 }"
		val expectedOutput = q"object A { val a = 12; val b = a + 13 }"
		val propagatedTree = (new BlockConstantPropagation[global.type]()(global)).apply(input, List())
		testTreeEquality(propagatedTree.tree, expectedOutput)
	}

	@Test def constantMethodTest: Unit = {
		val input = q"class A { def a = 12; def b = a + 13 }"
		val expectedOutput = q"class A { def a = 12; def b = a + 13 }"
		val propagatedTree = (new BlockConstantPropagation[global.type]()(global)).apply(input, List())
		testTreeEquality(propagatedTree.tree, expectedOutput)
	}

	@Test def blockFieldTest: Unit = {
		val input = q"class A { val a = { val c = 10; val d = 15; d }; val b = a + 25 }"
		val expectedOutput = q"class A { val a = 25; val b = 50 }"
		val propagatedTree = (new BlockConstantPropagation[global.type]()(global)).apply(input, List())
		testTreeEquality(propagatedTree.tree, expectedOutput)
	}

	@Test def blockReturnTest: Unit = {
		val input = q"class A { def m: Integer = { val a = 12; a + 13 } }"
		val expectedOutput = q"class A { def m: Integer = { val a = 12; 25 } }"
		val propagatedTree = (new BlockConstantPropagation[global.type]()(global)).apply(input, List())
		testTreeEquality(propagatedTree.tree, expectedOutput)
	}

	@Test def ifTest: Unit = {
		val input = q"class A { def m: Integer = { val a = 12; if (a < 13) 10 else 20} }"
		val expectedOutput = q"class A { def m: Integer = { val a = 12; 10 } }"
		val propagatedTree = (new BlockConstantPropagation[global.type]()(global)).apply(input, List())
		testTreeEquality(propagatedTree.tree, expectedOutput)
	}

	@Test def ifValTest: Unit = {
		val input = q"class A { val a = 12; val b = if (a < 13) 10 else 20 }"
		val expectedOutput = q"class A { val a = 12; val b = 10 }"
		val propagatedTree = (new BlockConstantPropagation[global.type]()(global)).apply(input, List())
		testTreeEquality(propagatedTree.tree, expectedOutput)
	}

	@Test def ifBlockTest: Unit = {
		val input = q"""class A { def m: Integer = { val a = 12; if (a < 13) println("true") else println("false") } }"""
		val expectedOutput = q"""class A { def m: Integer = { val a = 12; if (true) println("true") else println("false") } }"""
		val propagatedTree = (new BlockConstantPropagation[global.type]()(global)).apply(input, List())
		testTreeEquality(propagatedTree.tree, expectedOutput)
	}
}