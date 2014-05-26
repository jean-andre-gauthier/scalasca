package lara.epfl.scalasca.tests.unit.executables.intraproceduralcontrolflowgraph;
class DoWhileTest {
	var i = 0

	def m(): Unit = {
		do {
			println("still not 84")
			i += 1
		} while (i < 84)
	}
}