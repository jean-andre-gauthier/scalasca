class DivisionByZeroDefaultTest {
	def m(): Unit = {
		val z = 10 / 0
		val a = 10
		val b = 10 - a
		val c = 10 / b
	}
}