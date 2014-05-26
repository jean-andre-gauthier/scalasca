package lara.epfl.scalasca.tests.unit.executables.intraproceduralcontrolflowgraph;
class WhileTest {
	var i = 0

	def m(): Unit = {
		while (i < 42) {
			if (i % 2 == 0)
				println("even")
			println("still not 42")
			i += 1
		}
	}
}