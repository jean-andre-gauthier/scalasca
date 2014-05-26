
package lara.epfl.scalasca.tests.unit.executables.intraproceduralcontrolflowgraph;
class BasicTryTest {

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
		try {
			val d =
				if (b == 2) {
					println("Other Stuff")
					a + b
				}
				else {
					println("Some More Stuff")
					a * b
				}
			try {
				println("Throwing exception")
				if (a > 2)
					throw new Exception()
				else
					println("Actually not throwing exception")
			}
			finally {
				println("Didnt catch exception")
				println("Still idnt catch exception")
			}
			if (d == 20) {
				println("Some Last Stuff")
				return d + 21
			}
			d + 22
		}
		catch {
			case a: ArithmeticException =>
				println("Arithmetic exception")
			case r: RuntimeException =>
				println("Runtime exception")
			case e: Exception =>
				println("General exception")
		}
		finally {
			println("Inside Finally")
			println("Still Inside Finally")
		}
		println("Outside all tries")
		200
	}
}