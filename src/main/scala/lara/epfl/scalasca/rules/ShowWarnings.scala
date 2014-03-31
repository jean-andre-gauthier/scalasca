/*******************************************************************************
 *  _____           _       _____ _____   ___
 * /  ___|         | |     /  ___/  __ \ / _ \
 * \ `--.  ___ __ _| | __ _\ `--.| /  \// /_\ \
 *  `--. \/ __/ _` | |/ _` |`--. \ |    |  _  |
 * /\__/ / (_| (_| | | (_| /\__/ / \__/\| | | |
 * \____/ \___\__,_|_|\__,_\____/ \____/\_| |_/
 *
 * Static Code Analyser for Scala.
 * (c) 2014, LARA/EPFL, Typesafe
 *
 * Author: Jean Andre GAUTHIER
 * Supervisors: Dr. Viktor KUNCAK, Iulian DRAGOS
 ******************************************************************************/
package lara.epfl.scalasca.rules

import lara.epfl.scalasca.core._
import scala.tools.nsc._

//No RuleResult associated to this Rule

class ShowWarnings[T <: Global](implicit global: T) extends Rule[T]()(global) {

	import global._

	def apply(syntaxTree: Tree, computedResults: List[RuleResult]): NoResult = {
		computedResults.foreach(result => showRuleMessage(result))
		NoResult()
	}

	private def showRuleMessage(result: RuleResult) = result.warning match {
		case Fatal(_, _) =>
			Console.err.println(result.toString())
		case _ =>
			Console.println(result.toString())
	}
}
