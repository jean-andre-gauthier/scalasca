/*******************************************************************************
 * /**
 *  *  _____           _       _____ _____   ___
 *  * /  ___|         | |     /  ___/  __ \ / _ \
 *  * \ `--.  ___ __ _| | __ _\ `--.| /  \// /_\ \
 *  *  `--. \/ __/ _` | |/ _` |`--. \ |    |  _  |
 *  * /\__/ / (_| (_| | | (_| /\__/ / \__/\| | | |
 *  * \____/ \___\__,_|_|\__,_\____/ \____/\_| |_/
 *  *
 *  * Static Code Analyser for Scala.
 *  * (c) 2014, LARA/EPFL, Typesafe
 *  *
 *  * Author: Jean Andre GAUTHIER
 *  * Supervisors: Dr. Viktor KUNCAK, Iulian DRAGOS
 *  *
 *  */
 ******************************************************************************/

package scalasca.rules

import scalasca.core._
import scala.tools.nsc.Global
import Global._
import scala.tools.nsc.Settings


case class DivisionByZeroNodes(nodes: List[Global#Position]) extends RuleResult

object DivisionByZero extends Global(new Settings()) with Rule {

	override def fatal = false
	override def failureMessage = "Division by zero"
	override def name = "Division By Zero"

	override def apply(syntaxTree: Global#Tree, computedResults: List[RuleResult]): (Global#Tree, List[RuleResult]) = {
		val divisionByZeroNodes =
				for ( tree @ Apply(Select(rcvr, TermName("$div")), List(Literal(Constant(0)))) <- syntaxTree/*;
					if rcvr.tpe <:< typeOf[Int]*/)
					yield (tree.pos)
		divisionByZeroNodes.foreach(pos => showRuleFailure(pos))
		(syntaxTree,  DivisionByZeroNodes(divisionByZeroNodes) :: computedResults)
	}
}
