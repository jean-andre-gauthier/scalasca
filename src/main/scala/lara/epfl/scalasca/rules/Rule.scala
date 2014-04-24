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

object Rule {

	def apply[T <: Global](global: T)(tree: T#Tree, rules: List[Rule], debug: Boolean = false): List[RuleResult] = {

		val rulePositions = scala.collection.mutable.Map[Rule, Map[Option[Int], Rule#TS]]()
		for (rule <- rules) {
			rulePositions += (rule -> Map(Some(tree.id) -> rule.getDefaultState()))
		}

		def traverse(treeNode: T#Tree): Unit = {
			for (rule <- rules) {
				val currentNode = treeNode.asInstanceOf[rule.global.Tree]
				val currentNodePosition = treeNode.id

				rulePositions(rule).get(Some(currentNodePosition)) match {
					case Some(s) =>
						val currentState = rulePositions(rule).get(None) match {
							case Some(leafState) =>
								rule.mergeStates(leafState.asInstanceOf[rule.TS], s.asInstanceOf[rule.TS])
							case None =>
								s
						}
						val steppedPositionMap = rule.step(currentNode, currentState.asInstanceOf[rule.TS])
						rulePositions += (rule -> ((rulePositions(rule) - (None, Some(currentNodePosition))) ++ steppedPositionMap))
					case None =>
				}
			}
			treeNode.children.foreach(t => traverse(t))
		}
		traverse(tree)

		rulePositions.map(t => t._1.getRuleResult(t._2(None).asInstanceOf[t._1.TS])).toList.asInstanceOf[List[RuleResult]]
	}
}

abstract class Rule {

	type TS <: TraversalState
	type RR <: RuleResult

	val global: Global
	import global._

	def apply(syntaxTree: Tree, results: List[RuleResult]): RR

	def step(tree: Global#Tree, state: TS): Map[Option[Int], TS]

	def goto(tree: Global#Tree, state: TS): Map[Option[Int], TS] =
		if (!tree.isEmpty && tree.pos.isDefined){
			Map((Some(tree.id) -> state))}
		else
			Map(None -> state)
	def goto(trees: List[Global#Tree], state: TS, condition: Global#Tree => Boolean = _ => true): Map[Option[Int], TS] = trees.filter(condition).filter(t => !t.isEmpty && t.pos.isDefined) match {
		case Nil => Map(None -> state)
		case fTrees =>
			fTrees.foldLeft(Map[Option[Int], TS]())((acc, t) => acc + (Some(t.id) -> state))
	}
	def gotoChildren(tree: Global#Tree, state: TS): Map[Option[Int], TS] =
		goto(tree.children, state)
	def gotoChildren(trees: List[Global#Tree], state: TS, condition: Global#Tree => Boolean = _ => true): Map[Option[Int], TS] =
		goto(trees.flatMap(_.children), state, condition)
	def gotoLeaf(state: TS) = goto(Nil, state)

	def getRuleResult(state: TS): RR

	def getDefaultState(): TS
	def mergeStates(s1: TS, s2: TS): TS
}

abstract class TraversalState
case class NoState() extends TraversalState
