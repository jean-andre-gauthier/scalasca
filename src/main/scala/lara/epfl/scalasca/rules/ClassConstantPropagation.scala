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

case class ClassConstantPropagatedTree[T <: Global](tree: T#Tree) extends RuleResult {

	override def warning = Notice("Class Constant Propagation",
		"Propagating constant class fields (simple operations)",
		Console.GREEN + "No useless re-assignments found" + Console.RESET,
		GeneralCategory())

	override def toString: String = tree.toString

	//TODO
	override def isSuccess: Boolean = true
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
class ClassConstantPropagation[T <: Global](implicit global: T) extends Rule[T]()(global) {

	import global._

	object constants {
		private var constantsInScope = List[Set[(ValDef, Any)]]()

		def findConstant(termName: TermName): Option[Any] = {
			constantsInScope.
				filter(!_.filter(_._1.name == termName).isEmpty).
					headOption.map(opt => opt.filter(_._1.name == termName).headOption)
		}

		def addConstant(constantDefinition: ValDef, propagatedValue: Any): Unit = {
			if (constantsInScope.isEmpty)
				addBlockLevel
			constantsInScope.head + Tuple2(constantDefinition, propagatedValue)
		}

		def addBlockLevel: Unit =
			Set[(ValDef, Any)]() :: constantsInScope

		def removeBlockLevel =
			constantsInScope = constantsInScope match {
				case innermostBlock :: rest => rest
				case Nil => Nil
			}
	}

	private object transformer extends Transformer {

		override def transform(tree: Tree): Tree = {
			tree match {
				case ClassDef(mods, name, tparams, Template(parents, self, classMembers)) => {
					constants.addBlockLevel
					val ret = ClassDef(mods, name, tparams, Template(parents, self, super.transformTrees(classMembers)))
					constants.removeBlockLevel
					ret
				}
				case Block(stats, expr) => {
					constants.addBlockLevel
					val ret = Block(super.transformTrees(stats), expr)
					constants.removeBlockLevel
					ret
				}
				case constantVal @ ValDef(mods, name, tpt, Literal(Constant(value))) =>
					if (!mods.isMutable)
						constants.addConstant(constantVal, value)
					constantVal
				case value @ ValDef(mods, name, tpt, Apply(fun , args)) =>
					value //TODO
			}
		}

		private def evaluateToConstant(tree: Tree): Option[AnyVal] = tree match {
			case Apply(fun, args) => None //TODO
		}
	}

	def apply(syntaxTree: Tree, computedResults: List[RuleResult]): ClassConstantPropagatedTree[T] = {
		ClassConstantPropagatedTree(transformer.transform(syntaxTree))

		ClassConstantPropagatedTree(syntaxTree)
	}
}
