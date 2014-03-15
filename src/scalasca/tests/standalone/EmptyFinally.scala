package scalasca.tests.standalone

object EmptyFinally {
	def main(args: Array[String]) {
		try {
			println("In the try block")
		} catch {
		case e => println("In the catch block")
		}
		finally {
		}
	}
}