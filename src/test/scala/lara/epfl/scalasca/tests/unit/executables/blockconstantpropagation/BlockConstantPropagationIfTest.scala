class BCPIfTest {

	def m(): Int = {
		val a = 10
		val b = if (a < 11) 12 else 13
		if (a < 11) {
			println("ok")
			a
		}
		else if (a < 12) {
			println("not ok")
			a + 1
		}
		else {
			println("not ok at all")
			a + 2
		}
	}
}