package lara.epfl.scalasca.tests.unit.executables.intraproceduralcontrolflowgraph;

class ElementaryTryTest {

	private val a = 0
	private var b = 1

	def m(): Int = {
		try {
			println("Didnt catch exception")
			println("Still idnt catch exception")
		}
		catch {
			case e: Exception =>
				println("General exception")
		}
		finally {
			println("Inside Finally")
			println("Still Inside Finally")
		}
		200
	}
}