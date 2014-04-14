class BCPBooleanEqTest {
	def m(): Unit = {
		val a = true
		val b = a == true
		val c = a == false
		val d = false
		val e = d == true
		val f = d == false
		val g = d == 10
		val h = d == 10.0
		val i = d == "test"
	}
}