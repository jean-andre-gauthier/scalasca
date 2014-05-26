package lara.epfl.scalasca.tests.unit.executables.intraproceduralcontrolflowgraph;
class ForeachTest {
	var i = 0

	def m(): Unit = {
		val l = 1 :: 2 :: 5 :: 7 :: List(4)
		for (li <- l) {
			println(li)
			i += 1
			if (i % 2 == 0)
				println("even")
		}
	}
}