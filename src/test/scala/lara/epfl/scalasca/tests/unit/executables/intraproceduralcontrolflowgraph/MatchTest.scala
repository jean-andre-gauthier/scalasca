package lara.epfl.scalasca.tests.unit.executables.intraproceduralcontrolflowgraph;
class MatchTest {

	def m(): Unit = {
		val option: Option[Int] = Some(41)
		option match {
			case Some(o) =>
				println(option.getOrElse(if (o == 42) "43" else "44"))
			case None =>
				println("Nothing")
		}
	}
}