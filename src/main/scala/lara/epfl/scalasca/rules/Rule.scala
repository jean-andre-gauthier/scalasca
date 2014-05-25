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

	def apply(syntaxTree: Tree, results: List[RuleResult]): RR

}

abstract class StandardRule extends Rule

object ASTRule {
	type TT = Global#Tree

	def apply[T <: Global](global: T)(tree: TT, rules: List[ASTRule], debug: Boolean = false): List[RuleResult] = {
		val rulePositions = scala.collection.mutable.Map[ASTRule, Map[Option[Int], ASTRule#TS]]()
		for (rule <- rules) {
			rulePositions += (rule -> Map(Some(tree.hashCode()) -> rule.getDefaultState()))
		}

		def traverse(treeNode: TT): Unit = {
//			println(rulePositions)
			for (rule <- rules) {
				val currentNode = treeNode.asInstanceOf[rule.global.Tree]
				val currentNodeIdentifier = treeNode.hashCode()
//				println(currentNodeHashCode)
				rulePositions(rule).get(Some(currentNodeIdentifier)) match {
					case Some(s) =>
//					  println("s " + currentNodeHashCode)
						val currentState = rulePositions(rule).get(None) match {
							case Some(leafState) =>
								rule.mergeStates(leafState.asInstanceOf[rule.TS], s.asInstanceOf[rule.TS])
							case None =>
								s
						}
						val steppedPositionMap = rule.step(currentNode, currentState.asInstanceOf[rule.TS])
						rulePositions += (rule -> ((rulePositions(rule) - (None, Some(currentNodeIdentifier))) ++ steppedPositionMap))
//					  println("c " + rulePositions(rule))
					case None =>
//					  println("sn " + currentNodeHashCode)
				}
			}
			treeNode.children.foreach(t => traverse(t))
		}
		traverse(tree)

		rulePositions.map(t => t._1.getRuleResult(t._2(None).asInstanceOf[t._1.TS])).toList.asInstanceOf[List[RuleResult]]
	}
}

abstract class ASTRule extends Rule {

	type TS <: TraversalState
	type TT = Global#Tree

	def step(tree: TT, state: TS): Map[Option[Int], TS]

	def goto(tree: TT, state: TS): Map[Option[Int], TS] =
			Map((Some(tree.hashCode()) -> state))
	def goto(trees: List[TT], state: TS, condition: TT => Boolean = _ => true): Map[Option[Int], TS] = trees.filter(condition) match {
		case Nil =>
			Map(None -> state)
		case fTrees =>
			fTrees.foldLeft(Map[Option[Int], TS]())((acc, t) => acc + (Some(t.hashCode()) -> state))
	}
	def goto(treesStates: List[(TT, TS)], condition: TT => Boolean): Map[Option[Int], TS] =
		treesStates.foldLeft(Map[Option[Int], TS]())((acc, ts) => acc ++ (if (condition(ts._1)) goto(ts._1, ts._2) else Map()))
	def gotoChildren(tree: TT, state: TS): Map[Option[Int], TS] =
		goto(tree.children, state)
	def gotoChildren(trees: List[TT], state: TS, condition: TT => Boolean = _ => true): Map[Option[Int], TS] =
		goto(trees.flatMap(_.children), state, condition)
	def gotoLeaf(state: TS) = goto(Nil, state)

	def getRuleResult(state: TS): RR

	def getDefaultState(): TS
	def mergeStates(s1: TS, s2: TS): TS
}

abstract class TraversalState
case class NoState() extends TraversalState