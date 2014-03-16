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

case class DoubleTripleEqualsNodes(nodes: List[Global#Position]) extends RuleResult

/**
 * This is not clean, how to make it really generic e.g. other methods than === and ==?
 */
class DoubleTripleEquals extends Global(new Settings()) with Rule {

	override def fatal = false
	override def failureMessage = "Probably you intended to use === instead of =="
	override def name = "Double vs Triple Equals"

	override def apply(syntaxTree: Global#Tree, computedResults: List[RuleResult]): (Global#Tree, List[RuleResult]) = {
		val doubleTripleEquals =
				for ( tree @ Apply(Select(a, TermName("$eq$eq")), b :: Nil) <- syntaxTree;
					if a.tpe <:< typeOf[Actor] && b.tpe <:< typeOf[Actor])
				yield (tree.pos)
		doubleTripleEquals.foreach(pos => showRuleFailure(pos))
		(syntaxTree, DoubleTripleEqualsNodes(doubleTripleEquals) :: computedResults)
	}

	def apply[A, B](syntaxTree: Global#Tree, computedResults: List[RuleResult])(implicit tagA: TypeTag[A], tagB: TypeTag[B]): (Global#Tree, List[RuleResult]) = {
		val doubleTripleEquals =
				for ( tree @ Apply(Select(a, TermName("$eq$eq")), b :: Nil) <- syntaxTree;
					if a.tpe <:< tagA.tpe && b.tpe <:< tagB.tpe)
				yield (tree.pos)
		doubleTripleEquals.foreach(pos => showRuleFailure(pos))
		(syntaxTree, DoubleTripleEqualsNodes(doubleTripleEquals) :: computedResults)
	}
}
