package lara.epfl.scalasca.tests.unit.executables.intraproceduralcontrolflowgraph;
class MultipleMethodsTest {
	def m(): Int = {
		try {
			println("MultipleMethodsTest m1")
		}
		catch {
			case e: Exception =>
				println("MultipleMethodsTest m2")
				println("MultipleMethodsTest m3")
				return 42
		}
		finally {
			println("MultipleMethodsTest m4")
		}
		123
	}
	def n(): Unit = {
		var i = 0
		if (i < 1)
			println("MultipleMethodsTest n1")
		else
			println("MultipleMethodsTest n2")
	}
}