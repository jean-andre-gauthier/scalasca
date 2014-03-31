package lara.epfl.scalasca.rules;
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
//package scalasca.rules
//
//import scalasca.core._
//import scala.tools.nsc._
//import akka.actor.Actor
//
//case class DoubleTripleEqualsNodes(nodes: List[Global#Position]) extends RuleResult {
//
//	override def warning = Warning("Double vs Triple Equals", "Probable use of === instead of ==")
//
//	override def toString: String = nodes.foldLeft("")((acc, pos) => acc + "\n" + pos.showError(warning.toString()))
//}
//
///**
// * This is not clean, how to make it really generic e.g. other methods than === and ==?
// */
//class DoubleTripleEquals[T <: Global](implicit global: T) extends Rule[T]()(global) {
//
//	import global._
//
//	def apply[T <: Global](syntaxTree: T#Tree, computedResults: List[RuleResult])(implicit global: T): DoubleTripleEqualsNodes = {
//	  import global._
//		val doubleTripleEquals =
//				for ( tree @ Apply(Select(a, TermName("$eq$eq")), b :: Nil) <- syntaxTree;
//					if a.tpe <:< typeOf[Actor] && b.tpe <:< typeOf[Actor])
//				yield (tree.pos)
//		DoubleTripleEqualsNodes(doubleTripleEquals)
//	}
//
////	def apply[A, B](syntaxTree: Global#Tree, computedResults: List[RuleResult])(implicit tagA: TypeTag[A], tagB: TypeTag[B]): DoubleTripleEqualsNodes = {
////		val doubleTripleEquals =
////				for ( tree @ Apply(Select(a, TermName("$eq$eq")), b :: Nil) <- syntaxTree;
////					if a.tpe <:< tagA.tpe && b.tpe <:< tagB.tpe)
////				yield (tree.pos)
////		DoubleTripleEqualsNodes(doubleTripleEquals)
////	}
//}
