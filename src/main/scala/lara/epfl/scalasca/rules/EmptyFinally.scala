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

case class EmptyFinallyNodes(nodes: List[Global#Position]) extends RuleResult {

	override def warning = Warning("BLK_EMPTY_FINALLY",
		"Empty finally block",
		Console.GREEN + "No empty finally block found" + Console.RESET,
		BadPracticeCategory())

	override def toString: String =
		if (nodes.length > 0)
			nodes.foldLeft("")((acc, pos) => acc + "\n" + pos.showError(warning.formattedWarning))
		else
			warning.formattedWarning

	override def isSuccess: Boolean = nodes.length == 0
}

/**
 * BLK_EMPTY_FINALLY
 *
 * Finds empty finally blocks
 */
class EmptyFinally[T <: Global](implicit global: T) extends Rule[T]()(global) {

	import global._

	def apply(syntaxTree: Tree, computedResults: List[RuleResult]): EmptyFinallyNodes = {

		val emptyFinallys = for ( tree @ q"try $expr catch { case ..$cases } finally {}" <- syntaxTree) yield (tree.pos)
		EmptyFinallyNodes(emptyFinallys)
	}
}
