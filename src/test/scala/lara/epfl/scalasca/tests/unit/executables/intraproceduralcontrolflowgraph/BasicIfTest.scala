package lara.epfl.scalasca.tests.unit.executables.intraproceduralcontrolflowgraph;
class BasicIfTest {

	private val a = 0
	private var b = 1

	private object C {
		def c1(): Unit = {
			if (b == 2) {
				println("Stuff")
			}
		}
	}

	def m(): Int = {
		val d =
			if ((if (b == 2) 3 else 4) == 2) {
				println("Other Stuff")
				a + b
			}
			else {
				println("Some More Stuff")
				a * b
			}
		if (d == 20) {
			println("Some Last Stuff")
			return d + 21
		}
		d + 22
	}
}