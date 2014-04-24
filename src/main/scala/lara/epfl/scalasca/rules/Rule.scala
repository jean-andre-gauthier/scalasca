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
import scala.collection.mutable.Map

object Rule {

	def apply[T <: Global](global: T)(tree: T#Tree, rules: List[Rule], debug: Boolean = false): List[RuleResult] = {

		val rulePositions = Map[Rule, List[(Option[T#Position], Rule#TS)]]()
		for (rule <- rules) {
			rulePositions += (rule -> List((Some(tree.pos), rule.getDefaultState())))
		}

		for (treeNode <- tree) {
//			println(treeNode.id)
			for (rule <- rules) {
				val currentNode = treeNode.asInstanceOf[rule.global.Tree]
				val currentNodePosition = treeNode.pos.asInstanceOf[rule.global.Position]
				if (!rulePositions(rule).filter(_._1.forall(_ == currentNodePosition)).isEmpty) {
					val currentPositionTuple = rulePositions(rule).filter(_._1.forall(_ == currentNodePosition)).head
					val steppedPositionTuples = rule.step(currentNode, currentPositionTuple._2.asInstanceOf[rule.TS]) ::: rulePositions(rule).filter(_ != currentPositionTuple)
//					println(steppedPositionTuples)
					//Merging states having the same position
					val mergedStatesPositionTuples =  steppedPositionTuples.
						groupBy(_._1).
						map(t =>
							(t._1, t._2.map(_._2).reduceLeft((state1, state2) => rule.mergeStates(state1.asInstanceOf[rule.TS], state2.asInstanceOf[rule.TS])))).toList
					//Sorting by position
					val sortedPositionTuples = mergedStatesPositionTuples.sortWith((a, b) =>
						a._1 match {
							case Some(aPos) if aPos.isDefined => b._1 match {
								case Some(bPos) if bPos.isDefined =>
									if (aPos.line == bPos.line)
										aPos.column < bPos.column
									else
										aPos.line < bPos.line
//									aPos.point < bPos.point
								case _ => true
							}
							case _ => false})
//					println("\n++++++++++\n"+sortedPositionTuples.foldLeft("")((a, p) => (if (!p._1.isEmpty) a + p._1.get.pos.lineContent else a) + " " + p._2 + "\n")+"\n----------\n")
					val newPositionTuples: List[(Option[T#Position], Rule#TS)] =
					//Merging None position state into position at front of agenda
						if (!sortedPositionTuples.filter(_._1 == None).isEmpty) {
							val (nonePositionTuples, notNonePositionTuples) = sortedPositionTuples.partition(_._1 == None)
							if (!notNonePositionTuples.isEmpty)
								(notNonePositionTuples.head._1, rule.mergeStates(nonePositionTuples.head._2.asInstanceOf[rule.TS], notNonePositionTuples.head._2.asInstanceOf[rule.TS])) :: notNonePositionTuples.tail
							else
								nonePositionTuples
						}
						else
							sortedPositionTuples
					rulePositions += (rule -> newPositionTuples)
					if (debug) {
						val debugString: String = "\n----\n" + (for(t <- rulePositions(rule)) yield ("Position: " + (t._1 match { case Some(pos) => pos.lineContent case None => "None" }) + "\n+++++\nState: " + t._2 + "\n+++++\n")).mkString("")
						println(debugString)
					}
				}
			}
		}
		rulePositions.map(t => t._1.getRuleResult(t._2.map(_._2).reduceLeft((s1, s2) => t._1.mergeStates(s1.asInstanceOf[t._1.TS], s2.asInstanceOf[t._1.TS])).asInstanceOf[t._1.TS])).toList.asInstanceOf[List[RuleResult]]
	}
}

abstract class Rule {

	type TS <: TraversalState
	type RR <: RuleResult

	val global: Global
	import global._

	def apply(syntaxTree: Tree, results: List[RuleResult]): RR

	def step(tree: Global#Tree, state: TS): List[(Option[Position], TS)]

	def goto(tree: Global#Tree, state: TS): List[(Option[Position], TS)] = List((Some(tree.pos), state))
	def goto(trees: List[Global#Tree], state: TS, condition: Global#Tree => Boolean = _ => true): List[(Option[Position], TS)] = trees.filter(condition) match {
		case Nil => List((None, state))
		case _ => trees.map(t => (Some(t.pos), state))
	}
	def gotoChildren(tree: Global#Tree, state: TS): List[(Option[Position], TS)] =
		goto(tree.children, state)
	def gotoChildren(trees: List[Global#Tree], state: TS, condition: Global#Tree => Boolean = _ => true): List[(Option[Position], TS)] =
		goto(trees.flatMap(_.children), state, condition)
	def gotoLeaf(state: TS) = goto(Nil, state)

	def getRuleResult(state: TS): RR

	def getDefaultState(): TS
	def mergeStates(s1: TS, s2: TS): TS
}

abstract class TraversalState
case class NoState() extends TraversalState
