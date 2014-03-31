package lara.epfl.scalasca.rules

import lara.epfl.scalasca.core._
import scala.tools.nsc._
import reflect.runtime.universe._

case class BlockConstantPropagatedTree[T <: Global](tree: T#Tree, nPropagatedConstants: Integer) extends RuleResult {

	override def warning = Notice("Constant Propagation", "Propagating constants inside syntactic blocks (simple operations)")

	override def toString: String = warning.toString + Console.GREEN + " " + nPropagatedConstants + " values evaluated as constants" + Console.RESET// + "\n" + tree.toString()
}

/**
 * Considers:
 * 	- Values in simple expression blocks
 * 	- Any defined class field, provided a ClassConstantPropagatedMap is provided
 */
class BlockConstantPropagation[T <: Global](implicit global: T) extends Rule[T]()(global) with ConstantPropagationEvaluator {

	import global._

	private object transformer extends Transformer {

		private val variables =
			new VariablesInScope[global.type, Any]()(global)
		def nPropagatedConstants =
			variables.nFlaggedValues

		override def transform(tree: Tree): Tree = {
			tree match {
				case PackageDef(pid, stats) =>
					PackageDef(pid, super.transformTrees(stats))
				//Ignores class fields
				case ClassDef(mods, name, tparams, Template(parents, self, classMembers)) => {
					variables.addBlockLevel
					val newClassMembers = super.transformTrees(classMembers.filter(member => member.symbol.isMethod))
					variables.removeBlockLevel
					ClassDef(mods, name, tparams, Template(parents, self, newClassMembers))
				}
				//Ignores object fields
				case ModuleDef(mods, name, Template(parents, self, objectMembers)) => {
					variables.addBlockLevel
					val newObjectMembers = super.transformTrees(objectMembers.filter(tree => tree.symbol.isMethod))
					variables.removeBlockLevel
					ModuleDef(mods, name, Template(parents, self, newObjectMembers))
				}
				//Functions, provided they are more than a mere literal
				case functionDefinition @ DefDef(mods, name, tprarams, vparamss, tpt, rhs) => rhs match {
					case Block(_, _) => {
						variables.addBlockLevel
						val toReturn = DefDef(mods, name, tprarams, vparamss, tpt, super.transform(rhs))
						variables.removeBlockLevel
						toReturn
					}
					case _ =>
						functionDefinition
				}
				//Local block => add a block level
				case Block(stats, expr) => {
					variables.addBlockLevel
					val newStats = super.transformTrees(stats)
					val newExpr = super.transform(expr)
					variables.removeBlockLevel
					Block(newStats, newExpr)
				}
				//Block return value
				case apply @ Apply(fun, args) => {
					evaluateToConstant[global.type, Any](apply)(global)(variables) match {
						case Some(evaluatedConstant) => Literal(Constant(evaluatedConstant))
						case _ => apply
					}
				}
				//Constant val literal
				case constantVal @ ValDef(mods, name, tpt, Literal(Constant(constant))) =>
					if (!mods.isMutable && !constantVal.symbol.isParameter)
						variables.addFlagged(name, constant)
					constantVal
				//Constant val built from other constants
				case nonConstantVal @ ValDef(mods, name, tpt, application @ Apply(fun , args)) =>
					if (!mods.isMutable && !nonConstantVal.symbol.isParameter) {
						evaluateToConstant[global.type, Any](application)(global)(variables) match {
							case Some(constant) => {
								variables.addFlagged(name, constant)
								ValDef(mods, name, tpt, Literal(Constant(constant)))
							}
							case _ => nonConstantVal
						}
					}
					else
						nonConstantVal
				//Regular if
				case If(cond, thenP, elseP) => {
					val evaluatedCondOption = evaluateToConstant[global.type, Any](cond)(global)(variables)
					variables.addBlockLevel
					val evaluatedThen = super.transform(thenP)
					variables.removeBlockLevel; variables.addBlockLevel
					val evaluatedElse = super.transform(elseP)
					variables.removeBlockLevel
					evaluatedCondOption match {
						case Some(evaluatedCond) => If(Literal(Constant(evaluatedCond)), evaluatedThen, evaluatedElse)
						case None => If(cond, evaluatedThen, evaluatedElse)
					}
				}
				case anyOther => anyOther
			}
		}
	}

	def apply(syntaxTree: Tree, computedResults: List[RuleResult]): BlockConstantPropagatedTree[T] = {
		BlockConstantPropagatedTree(transformer.transform(syntaxTree), transformer.nPropagatedConstants)
	}
}