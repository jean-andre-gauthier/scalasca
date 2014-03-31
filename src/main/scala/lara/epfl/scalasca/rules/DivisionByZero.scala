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

	override def warning = SevereWarning("Division by zero", "Division By Zero")

	override def toString: String = nodes.foldLeft("")((acc, pos) => acc + "\n" + pos.showError(warning.toString))
}

class DivisionByZero[T <: Global](implicit global: T) extends Rule[T]()(global) {

	import global._

	def apply(syntaxTree: Tree, computedResults: List[RuleResult]): DivisionByZeroNodes = {

		val divisionByZeroNodes =
				for ( tree @ Apply(Select(rcvr, TermName("$div")), List(Literal(Constant(0)))) <- syntaxTree/*;
					if rcvr.tpe <:< typeOf[Int]*/)
					yield (tree.pos)
		DivisionByZeroNodes(divisionByZeroNodes)
	}
}
