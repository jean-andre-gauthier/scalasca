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


abstract class Rule {

	type RR <: RuleResult

	val ruleName: String

	val global: Global
	import global._

	def apply(syntaxTree: Tree): RR

}

abstract class StandardRule extends Rule

object ASTRule {
	type TT = Global#Tree

	def mergeNonePositions(rule: ASTRule, newPositions: List[(Option[TT], ASTRule#TS)]): List[(Option[TT], ASTRule#TS)] = {
		val (rulesNonePosition, rulesSomePosition) = newPositions.partition(_._1.isEmpty)
		val mergedNoneNewPositions: List[(Option[TT], ASTRule#TS)] =
			if (rulesNonePosition.size > 0) {
				val mergedNone =
					if (rulesNonePosition.size > 1)
						rulesNonePosition.tail.foldLeft(rulesNonePosition.head._2)((s, tuple) => rule.mergeStates(s.asInstanceOf[rule.TS], tuple._2.asInstanceOf[rule.TS]))
					else
						rulesNonePosition.head._2
				if (rulesSomePosition.size > 0)
					(rulesSomePosition.head._1, rule.mergeStates(mergedNone.asInstanceOf[rule.TS], rulesSomePosition.head._2.asInstanceOf[rule.TS])) :: rulesSomePosition.tail
				else
					List((None, mergedNone))
			}
			else
				rulesSomePosition
		mergedNoneNewPositions
	}

	def apply[T <: Global](global: T)(tree: TT, rules: List[ASTRule], debug: Boolean = false): List[RuleResult] = {
		val rulePositions = scala.collection.mutable.Map[ASTRule, List[(Option[TT], ASTRule#TS)]]()
		for (rule <- rules) {
			rulePositions += (rule -> List((Some(tree), rule.getDefaultState())))
		}
		while (rulePositions.exists(tuple => tuple._2.size >= 1 && !tuple._2.head._1.isEmpty)) {
			for (rule <- rules) {
				val currentEntry = rulePositions(rule)
				if (currentEntry.size >= 1 && !currentEntry.head._1.isEmpty) {
					val newPositions = rule.step(currentEntry.head._1.get, currentEntry.head._2.asInstanceOf[rule.TS]) ::: currentEntry.tail
					rulePositions += (rule -> mergeNonePositions(rule, newPositions))
				}
			}
		}
		rulePositions.map(t => t._1.getRuleResult(t._2.head._2.asInstanceOf[t._1.TS])).toList.asInstanceOf[List[RuleResult]]
	}
}

abstract class ASTRule extends Rule {

	type TS <: TraversalState
	type TT = Global#Tree

	def step(tree: TT, state: TS): List[(Option[TT], TS)]

	def goto(tree: TT, state: TS): List[(Option[TT], TS)] =
			List((Some(tree), state))
	def goto(trees: List[TT], state: TS, condition: TT => Boolean = _ => true): List[(Option[TT], TS)] = trees.filter(condition) match {
		case Nil =>
			List((None, state))
		case fTrees =>
			fTrees.foldLeft(List[(Option[TT], TS)]())((acc, t) => (Some(t), state) :: acc).reverse
	}
	def goto(treesStates: List[(TT, TS)], condition: TT => Boolean): List[(Option[TT], TS)] =
		treesStates.foldLeft(List[(Option[TT], TS)]())((acc, ts) => (if (condition(ts._1)) goto(ts._1, ts._2) else List()) ::: acc).reverse
	def gotoChildren(tree: TT, state: TS): List[(Option[TT], TS)] =
		goto(tree.children, state)
	def gotoChildren(trees: List[TT], state: TS, condition: TT => Boolean = _ => true): List[(Option[TT], TS)] =
		goto(trees.flatMap(_.children), state, condition)
	def gotoLeaf(state: TS) = goto(Nil, state)

	def getRuleResult(state: TS): RR

	def getDefaultState(): TS
	def mergeStates(s1: TS, s2: TS): TS
}

abstract class TraversalState
case class NoState() extends TraversalState