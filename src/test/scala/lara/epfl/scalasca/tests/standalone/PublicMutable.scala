package lara.epfl.scalasca.tests.standalone;
///*******************************************************************************
// *  _____           _       _____ _____   ___
// * /  ___|         | |     /  ___/  __ \ / _ \
// * \ `--.  ___ __ _| | __ _\ `--.| /  \// /_\ \
// *  `--. \/ __/ _` | |/ _` |`--. \ |    |  _  |
// * /\__/ / (_| (_| | | (_| /\__/ / \__/\| | | |
// * \____/ \___\__,_|_|\__,_\____/ \____/\_| |_/
// *
// * Static Code Analyser for Scala.
// * (c) 2014, LARA/EPFL, Typesafe
// *
// * Author: Jean Andre GAUTHIER
// * Supervisors: Dr. Viktor KUNCAK, Iulian DRAGOS
// ******************************************************************************/
//package scalasca.tests.standalone
//
//import akka.actor.Actor
//import akka.event.Logging
//
//class PublicMutable extends Actor{
//
//	val myValue = "my value"
//
//	val log = Logging(context.system, this)
//	def receive = {
//		case "test" => log.info("received test")
//		case _ => log.info("received unknown message")
//	}
//}
