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
import scala.collection.mutable.ListBuffer
import scala.tools.nsc._

case class MissingStringInterpolatorNodes(nodes: List[Global#Position]) extends RuleResult {

	override def warning = Warning("STR_MISSING_INTERPOLATOR",
		"String interpolation syntax without interpolator",
		Console.GREEN + "No dodgy strings found" + Console.RESET,
		GeneralCategory())

	override def toString: String =
		if (nodes.length > 0)
			nodes.foldLeft("")((acc, pos) => acc + "\n" + pos.showError(warning.formattedWarning))
		else
			warning.formattedWarning

	override def isSuccess: Boolean = nodes.length == 0
}

/**
 * STR_MISSING_INTERPOLATOR
 *
 * Flags strings containing a $ followed by a termname
 */
class MissingStringInterpolator[T <: Global](implicit global: T) extends Rule[T]()(global) {

	import global._

	private object traverser extends Traverser {

		private val _matched = ListBuffer[Position]()
		def matched = _matched.toList

		override def traverse(tree: Tree): Unit = tree match {
			case position @ Literal(const) =>
				if (const.stringValue.contains("$"))
					_matched += position.pos
			case _ => traverseTrees(tree.children)
		}
	}

	def apply(syntaxTree: Tree, computedResults: List[RuleResult]): MissingStringInterpolatorNodes = {

		traverser.traverse(syntaxTree)
		MissingStringInterpolatorNodes(traverser.matched)
	}
}
