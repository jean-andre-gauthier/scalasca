class BCPBooleanXorBooleanTest {
	def m(): Unit = {
		val a = true
		val b = a ^ true
		val c = a ^ false
		val d = false
		val e = d ^ true
		val f = d ^ false
	}
}