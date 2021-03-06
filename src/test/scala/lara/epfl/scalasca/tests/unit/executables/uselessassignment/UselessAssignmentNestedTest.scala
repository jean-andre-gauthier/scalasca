//class UANestedTest {
//
//	def m(): Unit = {
//		var m1 = 0
//		val m2 = 15
//		var m3 = "Some stuff"
//		var m4 = "Some other stuff"
//		var m5 = Some("Yet another stuff")
//		try {
//			class A(myInt: String) {
//				def a(): Unit = println(m1)
//				def b(): Unit = m1 = 12
//			}
//			val myA =
//				if (m2 > 15)
//					new A(m3)
//				else
//					new A(m4)
//		}
//		finally {
//			object B {
//				var a = if (m5 == Some("0")) 0 else 1
//				def b(b: String): Unit = {
//					a = 15
//					m5 = Some(".....")
//				}
//			}
//			trait C {
//				val a = m5
//				def b(): Unit = {
//					m5 = Some("+++++")
//					B.b(m5.get)
//				}
//			}
//		}
//	}
//}
//class UANested {
//	var b = 0
//	def m(): Unit = {
//		val a = "useless"
//	}
//}

class A {

	def m(): Unit = {
		val a = List(1, 2, 3)
		a.foldLeft("")((acc, i) => acc + " " + i.toString())
		for (b <- a) yield (b.toDouble)
		for (b <- a) {
			println(b)
		}
		val c = true
		while (c) {
			println(c)
		}
		do {
			println(c)
		} while (c)
	}
}