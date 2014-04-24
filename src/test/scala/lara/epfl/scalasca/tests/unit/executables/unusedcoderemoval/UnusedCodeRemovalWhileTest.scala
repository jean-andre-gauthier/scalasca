class UCRWhileTest {

	class A {
		def a(): Unit = {
			while (false) {
				println("a Dead code")
			}
			while (true)  {
				println("a Infinite loop")
			}
		}
	}

	def b(): Unit = {
		while (false) {
			println("b Dead code")
		}
		while (true)  {
			println("b Infinite loop")
		}
	}
}