package lara.epfl.scalasca.tests.standalone

class FreeResource {

	def main(args: Array[String]) {
		val file1 = scala.io.Source.fromFile("build.sbt")
		val a = 4
		if (a < 5) {
			file1.close()
			val file2 = scala.io.Source.fromFile("build.sbt")
			if (a < 4) {
				file2.close()
			}
			else {
				file2.close()
			}
		}
	}
}