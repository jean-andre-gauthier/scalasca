package scalasca.tests.standalone

import akka.actor.Actor
import akka.event.Logging

class PublicMutable extends Actor{

	val myValue = "my value"

	val log = Logging(context.system, this)
	def receive = {
		case "test" => log.info("received test")
		case _ => log.info("received unknown message")
	}
}