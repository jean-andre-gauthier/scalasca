package lara.epfl.scalasca.tests.unit.executables.unfreedresources

import scala.util.Random

class UnfreedResourcesMatchTest {

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
		b.openResource()
		c.openResource()
		a match {
			case Some(_) =>
				b.closeResource()
			case None =>
				b.closeResource()
				c.closeResource()
		}
	}
}