class EmptyFinalltDefaultTest {

	def m(): Unit = {

		object B {
			def b(): Unit = {
				try {
					println("BT")
				}
				catch {
					case e: Exception =>
						println("BE")
				}
				finally {
					println("BF")
				}
			}
		}

		trait C {
			def c(): Unit = {
				try {
					println("CT")
				} finally {}
			}
		}

		try {
			println("M1T")
		}
		catch {
			case e: Exception =>
				println("M1E")
		}

		try {
			println("M2T")
		}
		finally {
			println("M2F")
		}

		try {
			println("M3T")
		}
		finally {


		}
	}
}