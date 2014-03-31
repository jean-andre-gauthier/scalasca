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
//import akka.actor.Actor

case class PublicMutableFieldNodes(nodes: List[Global#Position]) extends RuleResult {

	override def warning = Notice("Public Mutable Fields", "Bad practice: public mutable fields")

	override def toString: String = nodes.foldLeft("")((acc, pos) => acc + "\n" + pos.showError(warning.toString()))
}

/**
 * How to check equality for flags?
 */
class PublicMutableField[T <: Global](implicit global: T) extends Rule[T]()(global) {

	import global._

	def apply(syntaxTree: Tree, computedResults: List[RuleResult]): PublicMutableFieldNodes = {

		val publicMutableFields =
			for (	tree @ ClassDef(_, _, _, Template(_, _, body)) <- syntaxTree;
					bodyTree @ ValDef(Modifiers(flags, _, _), _, _, _) <- body/*; if flags == MUTABLE*/)
					yield (tree.pos)
		PublicMutableFieldNodes(publicMutableFields)
	}
}
