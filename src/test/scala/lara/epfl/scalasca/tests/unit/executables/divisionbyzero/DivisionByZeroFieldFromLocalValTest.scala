class DivisionByZeroFieldFromLocalValTest {
	def m(): Unit = {
		val z = 10 / 0
		val a = 10
		val b = 10 - a
		val c = 10 / b
		class A {
			val a2 =
				if (11 / b < 12 / b)
					13
				else 14
		}
	}
}