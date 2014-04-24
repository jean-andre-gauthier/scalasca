class Reg1Test {

	def m(): Unit = {
		var m5 = Some("Yet another stuff")
		var a = m5 match {
			case Some(_) => 0
			case _ => 1
		}
		m5 match {
			case Some(_) => println("0")
			case _ => println("1")
		}
	}
}