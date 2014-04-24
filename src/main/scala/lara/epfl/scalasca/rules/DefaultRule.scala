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
//import scala.tools.nsc._
//
//class DefaultRule[T <: Global](global: T)(computedResults: List[RuleResult], sourcePath: String) extends Rule {
//
//	import global._
//
//	def apply(syntaxTree: Tree): NoResult = {
//
//		val divByZero = new DivisionByZero[global.type](global)(computedResults)
//		val emptyFinallys = new EmptyFinally[global.type](global)(computedResults)
//		val constProp = (new BlockConstantPropagation[global.type](global)).apply(syntaxTree, computedResults)
//		val codeRemoval = (new UnusedCodeRemoval[global.type](global)).apply(constProp.tree, computedResults)
//		val uselessAssignment = (new UselessAssignment[global.type](global)).apply(syntaxTree, computedResults)
//		val freeRessources = (new FreeResourceControlFlowAnalysis[global.type](global, q"scala.io.Source.fromFile", q"close")).apply(syntaxTree, computedResults)
//		(new ShowWarnings[global.type](global, sourcePath)).apply(syntaxTree, List(divByZero, constProp, codeRemoval, uselessAssignment, freeRessources))
//		NoResult()
//	}
//}
