package lara.epfl.scalasca.tests.unit.executables.unfreedresources

import scala.util.Random

class UnfreedResourcesIfTest {

	private class Resource {
		def openResource(): Unit = {
			println("open resource")
		}

		def closeResource(): Unit = {
			println("close resource")
		}
	}

	def m(): Unit = {
		val a = Random.nextBoolean()
		val b = new Resource()
		val c = new Resource()
		b.openResource()
		c.openResource()
		if (a) {
			b.closeResource()
		}
		else {
			b.closeResource()
			c.closeResource()
		}
	}
}