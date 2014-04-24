class UCRFieldIfTest {

	def m(): Unit = {
		val ab = true
		val bb = false
		class A {
			val a = if (ab) 11 else 12
		}

		object B {
			val b = if (!ab) 11 else 12
		}

		trait C {
			val c = if (ab && bb) 11 else 12
		}
		if (ab || bb)
			new A()
		else
			B
	}
}