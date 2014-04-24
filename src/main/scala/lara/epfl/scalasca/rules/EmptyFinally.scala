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

case class EmptyFinallyNodes(nodes: List[Global#Position]) extends RuleResult {

	override def warning = Warning("BLK_EMPTY_FINALLY",
		"Empty finally block",
		Console.GREEN + "No empty finally block found" + Console.RESET,
		BadPracticeCategory())

	override def toString: String =
		if (nodes.length > 0)
			nodes.foldLeft("")((acc, pos) => acc + "\n" + pos.showError(warning.formattedWarning))
		else
			warning.formattedWarning

	override def isSuccess: Boolean = nodes.length == 0
}

case class EmptyFinallyTraversalState(nodes: List[Global#Position]) extends TraversalState

/**
 * BLK_EMPTY_FINALLY
 *
 * Finds empty finally blocks
 */
class EmptyFinally[T <: Global](val global: T, computedResults: List[RuleResult] = List()) extends Rule {

	import global._

	type TS = EmptyFinallyTraversalState
	type RR = EmptyFinallyNodes

	override def getDefaultState(): TS = EmptyFinallyTraversalState(List())

	override def getRuleResult(state: TS): RR = EmptyFinallyNodes(state.nodes.sortBy(_.pos.point))

	override def mergeStates(s1: TS, s2: TS): TS =
			EmptyFinallyTraversalState((s1.nodes ::: s2.nodes).distinct)

	override def step(tree: Global#Tree, state: TS): List[(Option[Position], TS)] = tree match {
			case q"try $exprTry catch $cases finally {}" =>
				gotoChildren(tree, state.copy(nodes = tree.pos :: state.nodes))
			case q"try $exprTry finally {}" =>
				gotoChildren(tree, state.copy(nodes = tree.pos :: state.nodes))
			case _ =>
				gotoChildren(tree, state)
	}

	def apply(syntaxTree: Tree, computedResults: List[RuleResult] = List()): RR = {
		Rule.apply(global)(syntaxTree, List(this)) match {
			case result :: rest => result match {
				case e @ EmptyFinallyNodes(_) => e
				case _ => EmptyFinallyNodes(List())
			}
			case _ => EmptyFinallyNodes(List())
		}
	}
}
