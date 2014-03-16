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
import scala.tools.nsc._

case class EmptyFinallyNodes(nodes: List[Global#Position]) extends RuleResult

object EmptyFinally extends Global(new Settings()) with Rule {

	override def failureMessage = "Empty finally block"
	override def name = "Empty Finally"

	override def apply(syntaxTree: Global#Tree, computedResults: List[RuleResult]): (Global#Tree, List[RuleResult]) = {
		val emptyFinallys = for ( tree @ Try(block, catches, Literal(Constant(()))) <- syntaxTree) yield (tree.pos)
		emptyFinallys.foreach(pos => showRuleFailure(pos))
		(syntaxTree, EmptyFinallyNodes(emptyFinallys) :: computedResults)
	}
}
