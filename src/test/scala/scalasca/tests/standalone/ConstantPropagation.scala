package scalasca.tests.standalone

abstract class Z {
	val z = 13
	val blockVal = if (z < 12) {
		val blockInsideVal = 2
		blockInsideVal
	} else 2
}

class A {
	val a = new B();
	val b = 10;
	val c = (a.a + a.z + 14) * b;
	def m = { a.toString }
}

class B extends Z {
	class D {
		val a = 34
		val z = a
	}

	object e {
		val a = 1212 + z
	}

	val a = 1 + new D().a + e.a + z

	def b(bb: Int): Unit = {
		val f = 12.5;
		if (f+a < 15) {
			val insideB = 12312 + f
		}
		else f-a
		val g = 10
		val h = g / 4
		val i = "hello world"
		val j = i.length
		val k = j < 12
		val l = i.length > 15
		val m = k || l
		if (m)
			g + 10
		else
			g - 10
	}
}