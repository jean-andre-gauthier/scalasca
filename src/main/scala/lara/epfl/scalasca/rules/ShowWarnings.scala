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

class ShowWarnings[T <: Global](val global: T, source: String, inputResults: List[RuleResult] = List()) extends StandardRule {

	type TS = NoState
	type RR = NoResult

	override val ruleName = "GEN_SHOW_WARNINGS"

	import global._


	def apply(syntaxTree: Tree): RR = {
		if (inputResults.forall(res => res.isSuccess))
			println(Console.BOLD + source + Console.RESET + " ScalaSCA " + Console.GREEN + "No errors found" + Console.RESET)
		else {
			println(Console.BOLD + source + Console.RESET)
			inputResults.foreach(result => showRuleMessage(result))
		}
		NoResult()
	}

	private def showRuleMessage(result: RuleResult) = result.warning match {
		case Fatal(_, _, _, _) =>
			// Disabled for the moment Console.err.println(result.toString())
			Console.println(result.toString())
		case _ =>
			Console.println(result.toString())
	}
}
