import scala.actors.Actor

class DoubleTripleEqualsActorTest {
	class A extends Actor {
		def act = {}
		def === (a: Actor) = true
	}

	val a1 = new A()
	val a2 = new A()

	def m(): Unit = {
		a1 === a2
		class B {
			def b = a2 === a1
		}
	}
}