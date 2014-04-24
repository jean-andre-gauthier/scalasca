class UCRIfElseIfTest {
	def m(): Unit = {
		def n() {
			val d1 = 2.0
			val d2 = 2.5
			if (d2 > d1 + 0.5)
				println("test failed")
			else if (d2 >= d1 + 0.5)
				println("test successful")
			else
				println("test failed")
		}
	}
}