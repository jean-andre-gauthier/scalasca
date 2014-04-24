class PublicMutableFieldsClassTest {
	var myField0: Int = 0
	var myField1: Int = 0
	var myField1a: Int = 0
	var myField1b: String = "a_string"
	private var myField2: Int = 0
	val myField3: Int = 0
	private val myField4: Int = 0

	class A1 {
		var myField5: Int = 0
		var myField6: Int = 0
		private var myField7: Int = 0
		val myField8: Int = 0
		private val myField9: Int = 0

		def m1(): Unit = {
			class A2 {
				var myField10: Int = 0
			}
			var myVar1: Int = 0
		}
	}

	def m2(): Unit = {
		class A2 {
			var myField11: Int = 0
			var myField12: Int = 0
		}
		var myVar2: Int = 0
	}
}