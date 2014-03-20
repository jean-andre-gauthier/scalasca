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
package scalasca.rules

import scalasca.core._
import scala.tools.nsc._

class DefaultRule[T <: Global](implicit global: T) extends Rule[T]()(global) {

	import global._

	def apply(syntaxTree: Tree, computedResults: List[RuleResult] = List[RuleResult]()): NoResult = {

		val divByZero = (new DivisionByZero[global.type]()(global)).apply(syntaxTree, computedResults)
		val emptyFinallys = (new EmptyFinally[global.type]()(global)).apply(syntaxTree, computedResults)
		(new ShowWarnings[global.type]()(global)).apply(syntaxTree, List(divByZero, emptyFinallys))
		NoResult()
	}
}
