package lara.epfl.scalasca.tests.unit.executables.unfreedresources

import scala.util.Random

class UnfreedResourcesTryTest {

	class Resource {
		def openResource(): Unit = {
			println("open resource")
		}

		def closeResource(): Unit = {
			println("close resource")
		}
	}

	def m(): Unit = {
		val a: Option[Boolean] = Some(Random.nextBoolean())
		val b = new Resource()
		val c = new Resource()
		try {
			b.openResource()
			c.openResource()
		}
		catch {
			case e: ArithmeticException =>
			case e: NullPointerException =>
				b.closeResource()
		}
		finally {
			c.closeResource()
		}
	}
}