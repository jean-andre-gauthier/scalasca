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
import scala.collection.mutable._


case class UnreadValueVariableNodes(nodes: List[Global#Position]) extends RuleResult {

	override def warning = Notice("Unread Variable", "Val/var is never being used as rvalue")

	override def toString: String = nodes.foldLeft("")((acc, pos) => acc + "\n" + pos.showError(warning.toString()))
}

/**
 * Searches variables and values that are never read out after having been declared
 *
 * TODO
 * 		- Include vars
 * 		- How to generalise to fields?
 */
class UnreadValueVariable[T <: Global](implicit global: T) extends Rule[T]()(global) {

	import global._

	//TODO: change to Transformer
	private object traverser extends Traverser {
		/**
		 * variablesInScope's Boolean represents whether the value/variable has been read out
		 */
		private var variablesInScope = List[Set[(ValDef, Boolean)]]()
		private var _unreadVariables = Set[ValDef]()
		def unreadVariables = _unreadVariables.toList

		override def traverse(tree: Tree): Unit = {
			tree match {
				case Block(stats, expr) => {
					var valSet = Set[(ValDef, Boolean)]()

					stats.foreach(stat => {
						stat match {
							case valDef @ ValDef(_, _, _, _) => {
								valSet += Tuple2(valDef, false)
							}
							case _ =>
						}
						super.traverse(stat)
					})
					variablesInScope = valSet :: variablesInScope
					super.traverse(expr)
					_unreadVariables ++= variablesInScope.head.filter(!_._2).map(_._1)
					variablesInScope = variablesInScope.tail
				}
				case Select(Ident(termName @ TermName(_)), _) => {
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


	def apply(syntaxTree: Tree, computedResults: List[RuleResult]): UnreadValueVariableNodes = {

		traverser.traverse(syntaxTree)
		UnreadValueVariableNodes(for (tree <- traverser.unreadVariables) yield (tree.pos))
	}
}
