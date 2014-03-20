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

case class ConstantPropagatedTree(tree: Global#Tree) extends RuleResult {

	override def warning = Notice("Constant Propagation", "Propagating constants (simple operations)")

	override def toString: String = tree.toString
}

/**
 * Basic constant propagation.
 *
 * Takes into account:
 * 		- Class/object fields
 * 		- Local vals/vars
 *
 * TODO:
 * 		- Intraprocedural analysis
 * 		- Discuss forward references
 * 		- Add vars
 */
class ConstantPropagation[T <: Global](implicit global: T) extends Rule[T]()(global) {

	import global._
//
//	object constants {
//		private var constantsInScope = List[Set[(Global#ValDef, Any)]]()
//
//		def findConstant(termName: TermName): Option[Any] = {
//			constantsInScope.
//				filter(!_.filter(_._1.name == termName).isEmpty).
//					headOption.map(opt => opt.filter(_._1.name == termName).headOption)
//		}
//
//		def addConstant(constantDefinition: Global#ValDef, propagatedValue: Any): Unit = {
//			if (constantsInScope.isEmpty)
//				addBlockLevel
//			constantsInScope.head + Tuple2(constantDefinition, propagatedValue)
//		}
//
//		def addBlockLevel =
//			Set[(Global#ValDef, Any)]() :: constantsInScope
//
//		def removeBlockLevel =
//			constantsInScope = constantsInScope match {
//				case innermostBlock :: rest => rest
//				case Nil => Nil
//			}
//	}

//	object transformer /*extends Transformer*/ {
//
//		override def transform(tree: Tree): Global#Tree = {
//			tree match {
//				case ClassDef(mods, name, tparams, Template(parents, self, classMembers)) => {
//					constants.addBlockLevel
//					val ret = ClassDef(mods, name, tparams, Template(parents, self, super.transformTrees(classMembers)))
//					constants.removeBlockLevel
//					ret
//				}
//				case Block(stats, expr) => {
//					constants.addBlockLevel
//					val ret = Block(super.transformTrees(stats), expr)
//					constants.removeBlockLevel
//					ret
//				}
//				case constantVal @ ValDef(_, _, _, Literal(Constant(value))) =>
//					constants.addConstant(constantVal, value)
//					//constantVal.symbol.is
//			}
//		}
//	}

	//TODO What should this rule return?
	def apply(syntaxTree: Tree, computedResults: List[RuleResult]): ConstantPropagatedTree = {
//		ConstantPropagatedTree(transformer.transform(syntaxTree))

		ConstantPropagatedTree(syntaxTree)
	}
}
