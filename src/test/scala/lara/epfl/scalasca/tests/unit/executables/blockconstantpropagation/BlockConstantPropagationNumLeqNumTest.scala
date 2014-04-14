class BCPNumLeqNumTest {
	def m(): Unit = {
		val a = 10
		val b = a <= 9
		val c = a <= 10
		val d = a <= 10.0
		val e = a <= 11
		val f = 10.0
		val g = f <= 9.0
		val h = f <= 10.0
		val i = f <= 10
		val j = f <= 11.0
	}
}