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
import akka.actor.Actor

case class PublicMutableFieldNodes(nodes: List[Global#Position]) extends RuleResult

/**
 * How to check equality for flags?
 */
object PublicMutableField extends Global(new Settings()) with Rule {

	override def fatal = false
	override def failureMessage = "Bad practice: public mutable fields"
	override def name = "Public Mutable Fields"

	override def apply(syntaxTree: Global#Tree, computedResults: List[RuleResult]): (Global#Tree, List[RuleResult]) = {
		val publicMutableFields =
			for (	tree @ ClassDef(_, _, _, Template(_, _, body)) <- syntaxTree;
					bodyTree @ ValDef(Modifiers(flags, _, _), _, _, _) <- body/*; if flags == MUTABLE*/)
					yield (tree.pos)
		publicMutableFields.foreach(pos => showRuleFailure(pos))
		(syntaxTree, PublicMutableFieldNodes(publicMutableFields) :: computedResults)
	}
}
