package lara.epfl.scalasca.tests.unit.executables.unfreedresources

import scala.util.Random

class UnfreedResourcesWhileTest {

	private class Resource {
		def openResource(): Unit = {
			println("open resource")
		}

		def closeResource(): Unit = {
			println("close resource")
		}
	}

	def m(): Unit = {
		var a = Random.nextInt() < 1
		val b = new Resource()
		val c = new Resource()
		while (a) {
			b.closeResource()
			a = Random.nextBoolean()
		}
		c.closeResource()
	}
}
