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

class ShowWarnings[T <: Global](val global: T, source: String) extends Rule {

	type TS = NoState
	type RR = NoResult

	import global._

	override def getRuleResult(state: TS): RR = NoResult()
	override def step(tree: Global#Tree, state: TS): Map[Option[Int], TS] = Map[Option[Int], TS]()
	override def getDefaultState(): TS = NoState()
	override def mergeStates(s1: TS, s2: TS): TS = NoState()

	def apply(syntaxTree: Tree, computedResults: List[RuleResult]): RR = {
		if (computedResults.forall(res => res.isSuccess))
			println(Console.BOLD + source + Console.RESET + " ScalaSCA " + Console.GREEN + "No errors found" + Console.RESET)
		else {
			println(Console.BOLD + source + Console.RESET)
			computedResults.foreach(result => showRuleMessage(result))
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
