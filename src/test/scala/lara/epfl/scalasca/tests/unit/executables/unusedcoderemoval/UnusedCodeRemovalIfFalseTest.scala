class UCRIfFalseTest {
	def m(): Unit = {
		val s1 = "a string"
		val s2 = "b string"

		if (s1 == s2)
			println("test failed")
		else
			println("test successful")
	}
}