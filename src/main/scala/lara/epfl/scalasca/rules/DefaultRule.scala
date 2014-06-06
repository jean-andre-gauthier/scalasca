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

class DefaultRule[T <: Global](val global: T, sourcePath: String, inputResults: List[RuleResult] = List()) extends Rule {

	import global._

	type RR = NoResult

	override val ruleName = "GEN_DEFAULT"

	def apply(syntaxTree: Tree): NoResult = {

//		val constProp = (new BlockConstantPropagation[global.type](global, inputResults)).apply(syntaxTree)
//		val cfg = (new IntraProceduralControlFlowGraphGenerator[global.type](global, inputResults)).apply(syntaxTree)
//		val intermediateResults: List[RuleResult] = constProp :: List(cfg)

		val intermediateResults: List[RuleResult] = List()

//		val divByZero = (new DivisionByZero[global.type](global, intermediateResults)).apply(syntaxTree)
//		val emptyFinallys = (new EmptyFinally[global.type](global, intermediateResults)).apply(syntaxTree)
//		val doubleTripleEquals = (new DoubleTripleEquals[global.type, Actor](global, intermediateResults)).apply(syntaxTree)
		val unfreedResources = (new UnfreedResourcesControlFlowAnalysis[global.type](global, TermName("open"), TermName("close"), intermediateResults)).apply(syntaxTree)
//		val codeRemoval = new UnusedCodeRemoval[global.type](global, intermediateResults)
//		val uselessAssignment = new UselessAssignment[global.type](global, intermediateResults)
//		val finalResults = ASTRule.apply(global)(syntaxTree, List(divByZero, emptyFinallys, doubleTripleEquals))
//		val freeRessources = (new FreeResourceControlFlowAnalysis[global.type](global, q"scala.io.Source.fromFile", q"close")).apply(syntaxTree, computedResults)
//		(new ShowWarnings[global.type](global, sourcePath, finalResults)).apply(syntaxTree)
		NoResult()
	}
}
