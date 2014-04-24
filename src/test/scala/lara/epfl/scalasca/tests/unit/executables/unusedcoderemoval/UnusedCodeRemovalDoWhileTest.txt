class UCRDoWhileTest {

	class A {
		def a(): Unit = {
			do {
				println("a No need for a loop")
			} while (false)
			do {
				println("a Infinite loop")
			} while (true)
		}
	}

	def b(): Unit = {
		do {
			println("a No need for a loop")
		} while (false)
		do {
			println("a Infinite loop")
		} while (true)
	}
}