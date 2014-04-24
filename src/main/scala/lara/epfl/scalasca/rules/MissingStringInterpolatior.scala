///*******************************************************************************
// *  _____           _       _____ _____   ___
// * /  ___|         | |     /  ___/  __ \ / _ \
// * \ `--.  ___ __ _| | __ _\ `--.| /  \// /_\ \
// *  `--. \/ __/ _` | |/ _` |`--. \ |    |  _  |
// * /\__/ / (_| (_| | | (_| /\__/ / \__/\| | | |
// * \____/ \___\__,_|_|\__,_\____/ \____/\_| |_/
// *
// * Static Code Analyser for Scala.
// * (c) 2014, LARA/EPFL, Typesafe
// *
// * Author: Jean Andre GAUTHIER
// * Supervisors: Dr. Viktor KUNCAK, Iulian DRAGOS
// ******************************************************************************/
//package lara.epfl.scalasca.rules
//
//import lara.epfl.scalasca.core._
//import scala.collection.mutable.ListBuffer
//import scala.tools.nsc._
//
//case class MissingStringInterpolatorNodes(nodes: List[Global#Position]) extends RuleResult {
//
//	override def warning = Warning("STR_MISSING_INTERPOLATOR",
//		"String interpolation syntax without interpolator",
//		Console.GREEN + "No dodgy strings found" + Console.RESET,
//		GeneralCategory())
//
//	override def toString: String =
//		if (nodes.length > 0)
//			nodes.foldLeft("")((acc, pos) => acc + "\n" + pos.showError(warning.formattedWarning))
//		else
//			warning.formattedWarning
//
//	override def isSuccess: Boolean = nodes.length == 0
//}
//
///**
// * STR_MISSING_INTERPOLATOR
// *
// * Flags strings containing a $ followed by a termname
// */
//class MissingStringInterpolator[T <: Global](val global: T) extends Rule {
//
//	import global._
//
//	type TS = DoubleTripleEqualsTraversalState
//	type RR = DoubleTripleEqualsNodes
//
//	override def getDefaultState(): TS = PublicMutableFieldsTraversalState(false, List())
//
//	override def getRuleResult(state: TS): RR = PublicMutableFieldsNodes(state.mutableFields.sortBy(_.pos.point))
//
//	override def mergeStates(s1: TS, s2: TS): TS =
//			PublicMutableFieldsTraversalState(s1.inMembers || s2.inMembers, (s1.mutableFields ::: s2.mutableFields).distinct)
//
//	override def step(tree: Global#Tree, state: TS): List[(Option[Position], TS)] = tree.asInstanceOf[Tree] match {
//			case position @ Literal(const) =>
//				if (const.stringValue.contains("$"))
//					_matched += position.pos
//				List()
//			case _ =>
//				tree.children.map(c => c.pos)
//	}
//
//	def apply(syntaxTree: Tree, computedResults: List[RuleResult]): RR = {
//		Rule.apply(global)(syntaxTree, List(this)) match {
//			case result :: rest => result match {
//				case m @ MissingStringInterpolatorNodes(_) => m
//				case _ => MissingStringInterpolatorNodes(List())
//			}
//			case _ => MissingStringInterpolatorNodes(List())
//		}
//	}
//}
