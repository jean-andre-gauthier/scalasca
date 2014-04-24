class UAFlatTest {

	def m(): Unit = {
		var a = 0
		val b = 15
		var c = "Some stuff"
		try {
			println(c)
		}
		finally {
			a = 10
			println(a + b)
			c = "Useless assignment"
		}
	}
}