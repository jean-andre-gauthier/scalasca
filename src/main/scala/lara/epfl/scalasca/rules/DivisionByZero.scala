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


case class DivisionByZeroNodes(nodes: List[Global#Position]) extends RuleResult {

	override def warning = Fatal("ARI_DIV_BY_ZERO",
		"Division By Zero",
		Console.GREEN + "No division by 0 found" + Console.RESET,
		FatalCategory())

	override def toString: String =
		if (nodes.length > 0)
			nodes.foldLeft("")((acc, pos) => acc + "\n" + pos.showError(warning.formattedWarning))
		else
			warning.formattedDefaultMessage

	override def isSuccess: Boolean = nodes.length == 0
}

case class DivisionByZeroState(nodes: List[Global#Position]) extends TraversalState

/**
 * ARI_DIV_BY_ZERO GEN_BLOCK_CONST_PROP
 *
 * Finds explicit divisions by 0
 */
class DivisionByZero[T <: Global](val global: T, inputResults: List[RuleResult] = List()) extends Rule with ConstantPropagationEvaluator {

	import global._

	type TS = DivisionByZeroState
	type RR = DivisionByZeroNodes

	override def getDefaultState(): TS = DivisionByZeroState(List())

	override def getRuleResult(state: TS): RR = DivisionByZeroNodes(state.nodes.sortBy(_.pos.point))

	override def mergeStates(s1: TS, s2: TS): TS =
			DivisionByZeroState((s1.nodes ::: s2.nodes).distinct)

	private val inputSymbolMap = SymbolMapper.getLiteralMapping(inputResults)

	override def step(tree: Global#Tree, state: TS): Map[Option[Int], TS] = tree match {
			case Apply(Select(rcvr, TermName("$div")), List(denominator)) if rcvr.tpe <:< typeOf[Int] =>
				val computedDenominator =
					if (inputSymbolMap.isEmpty)
						denominator match {
							case Literal(Constant(0)) => Some(0)
							case _ => None
						}
					else
						evaluateToConstant(denominator)(global)(inputSymbolMap) match {
							case Some(constant) => Some(constant.asInstanceOf[Int])
							case _ => None
						}
				computedDenominator match {
					case Some(value) if value == 0 =>
						gotoLeaf(state.copy(nodes = tree.pos :: state.nodes))
					case _ =>
						gotoChildren(List(rcvr, denominator), state)
				}
			case _ =>
				gotoChildren(tree, state)
	}

	override def apply(syntaxTree: Tree, computedResults: List[RuleResult]): RR = {
		Rule.apply(global)(syntaxTree, List(this)) match {
			case result :: rest => result match {
				case d @ DivisionByZeroNodes(_) => d
				case _ => DivisionByZeroNodes(List())
			}
			case _ => DivisionByZeroNodes(List())
		}
	}
}
