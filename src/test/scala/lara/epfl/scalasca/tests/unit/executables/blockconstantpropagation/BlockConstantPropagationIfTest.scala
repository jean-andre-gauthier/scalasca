class BCPIfTest {

	def m(): Int = {
		val a = 10
		val b = if (a < 11) 12 else 13
		b
	}
}