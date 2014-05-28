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
import scala.actors.Actor
import scala.reflect.runtime.universe.TypeTag

case class DoubleTripleEqualsNodes(nodes: List[Global#Symbol]) extends RuleResult {

	override def warning = Warning(
		"MET_DOUBLE_TRIPLE_EQUALS",
		"Probable use of == instead of ===",
		Console.GREEN + "No ambiguous use of == and ===",
		GeneralCategory())

	override def toString: String = nodes.foldLeft("")((acc, sym) => acc + "\n" + sym.pos.showError(warning.toString()))
}

case class DoubleTripleEqualsTraversalState(nodes: List[Global#Symbol]) extends TraversalState

/**
 * This is not clean, how to make it really generic e.g. other methods than === and ==?
 */
class DoubleTripleEquals[T <: Global, U](val global: T, inputResults: List[RuleResult] = List())(implicit tagU: TypeTag[U]) extends ASTRule {

	import global._

	type TS = DoubleTripleEqualsTraversalState
	type RR = DoubleTripleEqualsNodes

	override val ruleName = "MET_DOUBLE_TRIPLE_EQUALS"

	override def getDefaultState(): TS = DoubleTripleEqualsTraversalState(List())

	override def getRuleResult(state: TS): RR = DoubleTripleEqualsNodes(state.nodes.sortBy(_.pos.point))

	override def mergeStates(s1: TS, s2: TS): TS =
			DoubleTripleEqualsTraversalState((s1.nodes ::: s2.nodes).distinct)

	override def step(tree: Global#Tree, state: TS): List[(Option[TT], TS)] = tree.asInstanceOf[Tree] match {
		case Apply(Select(a, TermName("$eq$eq$eq")), b :: Nil) if a.tpe <:< typeOf[U] && b.tpe <:< typeOf[U] =>
			goto(Nil, DoubleTripleEqualsTraversalState(a.symbol :: state.nodes))
		case _ =>
			goto(tree.children, state)
	}

	override def apply(syntaxTree: Tree): RR = {
		ASTRule.apply(global)(syntaxTree, List(this)) match {
			case result :: rest => result match {
				case d @ DoubleTripleEqualsNodes(_) => d
				case _ => DoubleTripleEqualsNodes(List())
			}
			case _ => DoubleTripleEqualsNodes(List())
		}
	}
}
