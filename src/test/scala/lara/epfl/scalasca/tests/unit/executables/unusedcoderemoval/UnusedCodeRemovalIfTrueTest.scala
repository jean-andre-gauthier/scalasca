class UCRIfTrueTest {
	def m(): Unit = {
		val s = "a string"
		if (s.length() == 8)
			println("test successful")
		else
			println("test failed")
	}
}