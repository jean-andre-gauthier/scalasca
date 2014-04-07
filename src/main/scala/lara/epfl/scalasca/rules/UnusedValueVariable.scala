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
import scala.collection.mutable._


case class UnusedValueVariableNodes(nodes: List[Global#Position]) extends RuleResult {

	override def warning = Notice("DEC_UNUSED_DATA", "Unused val/var", "", BadPracticeCategory())

	override def toString: String = nodes.foldLeft("")((acc, pos) => acc + "\n" + pos.showError(warning.formattedWarning))

	override def isSuccess: Boolean = nodes.length == 0
}

/**
 * DEC_UNUSED_DATA
 *
 * Searches variables and values that are never used / only written to after their declaration.
 *
 * DOES NOT filter out:
 * 		- Fields
 *
 * TODO
 * 		- Include vars
 * 		- Values that are only written to
 * 		- How to generalise to fields?
 */
class UnusedValueVariable[T <: Global](implicit global: T) extends Rule[T]()(global) {

	import global._

	//TODO: change to Transformer
	private object traverser extends Traverser {
		/**
		 * variablesInScope's Boolean represents whether the value/variable has been used
		 */
		private var variablesInScope = List[Set[(ValDef, Boolean)]]()
		private var _unusedVariables = Set[ValDef]()
		def unusedVariables = _unusedVariables.toList

		override def traverse(tree: Tree): Unit = {
			tree match {
				case Block(stats, expr) => {
					var valSet = Set[(ValDef, Boolean)]()

					stats.foreach(stat => {
						stat match {
							case valDef @ ValDef(mods, _, _, _) => {
								if (!mods.isMutable)
									valSet += Tuple2(valDef, false)
							}
							case _ =>
						}
						super.traverse(stat)
					})
					variablesInScope = valSet :: variablesInScope
					super.traverse(expr)
					_unusedVariables ++= variablesInScope.head.filter(!_._2).map(_._1)
					variablesInScope = variablesInScope.tail
				}
				case Ident(termName @ TermName(_)) => {
					variablesInScope.
						filter(!_.filter(_._1.name == termName).isEmpty).
						headOption.foreach(s => s.foreach(v =>
							if (v._1.name == termName) {
								s.update(v, false)
								s += Tuple2(v._1, true)
							}
							else
								v
							))
				}
				case _ =>
			}
			super.traverseTrees(tree.children)
		}
	}

	def apply(syntaxTree: Tree, computedResults: List[RuleResult]): UnusedValueVariableNodes = {

		traverser.traverse(syntaxTree)
		UnusedValueVariableNodes(for (tree <- traverser.unusedVariables) yield (tree.pos))
	}
}
