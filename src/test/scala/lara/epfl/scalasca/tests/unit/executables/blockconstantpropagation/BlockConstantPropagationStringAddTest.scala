class BCPStringAddTest {
	def m(): Unit = {
		val a = "test"
		val b = a + 15
		val c = a + 11.5
		val d = a + false
		val e = a + a
	}
}