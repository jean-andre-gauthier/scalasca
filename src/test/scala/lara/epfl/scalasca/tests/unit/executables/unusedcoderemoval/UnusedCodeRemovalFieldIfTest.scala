class UCRFieldIfTest {

	class A {
		val a = if (true) 11 else 12
	}

	object B {
		val b = if (true) 11 else 12
	}

	trait C {
		val c = if (true) 11 else 12
	}
}