package lara.epfl.scalasca.rules

import lara.epfl.scalasca.core._
import scala.tools.nsc._
import reflect.runtime.universe._
import scala.tools.reflect.ToolBox

case class BlockConstantPropagatedTree[T <: Global](tree: T#Tree, nPropagatedConstants: Integer) extends RuleResult {

	override def warning = Notice("GEN_BLOCK_CONST_PROP",
		"Propagating constants inside syntactic blocks (simple operations)",
		Console.GREEN + "No constants to be propagated" + Console.RESET,
		GeneralCategory())

	override def toString: String =
		if (nPropagatedConstants > 0)
			warning.formattedWarning + Console.BLUE + " " + nPropagatedConstants + " values evaluated as constants" + Console.RESET
		else
			warning.formattedDefaultMessage

	override def isSuccess: Boolean = nPropagatedConstants == 0
}

/**
 * GEN_BLOCK_CONST_PROP
 *
 * Considers:
 * 	- Values in simple expression blocks
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
				case q"package $ref { ..$stats }" =>
					val newStats: List[Tree] = transformTrees(stats)
					q"package $ref { ..$newStats }"
				//Ignores class fields
				case q"$mods class $tpname[..$targs] $ctorMods(...$paramss) extends { ..$early } with ..$parents { $self => ..$stats }" => {
					val newStats: List[Tree] = stats.map(member => member match {
						case q"$mods def $tname[..$targs](...$paramss): $tpt = $expr" => transform(member)
						case _ => member})
					q"$mods class $tpname[..$targs] $ctorMods(...$paramss) extends { ..$early } with ..$parents { $self => ..$newStats }"
				}
				//Ignores object fields
				case q"$mods object $tname extends { ..$early } with ..$parents { $self => ..$body }" => {
					val newBody: List[Tree] = body.map(member => member match {
						case q"$mods def $tname[..$targs](...$paramss): $tpt = $expr" => transform(member)
						case _ => member})
					q"$mods object $tname extends { ..$early } with ..$parents { $self => ..$newBody }"
				}
				//Ignores trait fields
				case q"$mods trait $tpname[..$tparams] extends { ..$earlydefns } with ..$parents { $self => ..$stats }" => {
					val newStats: List[Tree] = stats.map(member => member match {
						case q"$mods def $tname[..$targs](...$paramss): $tpt = $expr" => transform(member)
						case _ => member})
					q"$mods trait $tpname[..$tparams] extends { ..$earlydefns } with ..$parents { $self => ..$newStats }"
				}
				//Functions, provided they are more than a mere literal
				case functionDefinition @ q"$mods def $tname[..$targs](...$paramss): $tpt = $expr" => expr match {
					case Block(_, _) => {
						val newExpr = transform(expr)
						q"$mods def $tname[..$targs](...$paramss): $tpt = $newExpr"
					}
					case _ =>
						functionDefinition
				}
				//Regular if
				case q"if ($cond) $thenP else $elseP" => {
					val evaluatedCondOption = evaluateToConstant[global.type, Any](cond)(global)(variables)
					val evaluatedThen = transform(thenP)
					val evaluatedElse = transform(elseP)
					evaluatedCondOption match {
						case Some(evaluatedCond) => {
							if (evaluatedCond.isInstanceOf[Boolean]) {
								val booleanCond = evaluatedCond.asInstanceOf[Boolean]
								q"if ($booleanCond) $evaluatedThen else $evaluatedElse"
							}
							else q"if ($cond) $evaluatedThen else $evaluatedElse"
						}
						case None => q"if ($cond) $evaluatedThen else $evaluatedElse"
					}
				}
				case constantVal @ q"$mods val $tname: $tpt = $expr" => expr match {
					//Constant val literal
					case cst @ Literal(Constant(constant)) => {
						println(">>>" + tree.symbol)
						variables.addFlagged(constantVal.symbol, constant)
						constantVal
					}
					//Constant val built from other constants
					case application @ q"$fun(...$args)" =>
						evaluateToConstant[global.type, Any](application)(global)(variables) match {
							case Some(constant) => {
								if (constant.isInstanceOf[Int]) {
									val intConstant = constant.asInstanceOf[Int]
									variables.addFlagged(constantVal.symbol, constant)
									q"$mods val $tname: $tpt = $intConstant"
								}
								else if (constant.isInstanceOf[String]) {
									val stringConstant = constant.asInstanceOf[String]
									variables.addFlagged(constantVal.symbol, constant)
									q"$mods val $tname: $tpt = $stringConstant"
								}
								else constantVal
							}
							case c => constantVal
						}
					case _ => constantVal
				}
				//Local block => add a block level
				case block @ q"{ ..$stats }" => stats match {
					case stat :: Nil => stat match {
						case application @ q"$fun(...$args)" =>
							evaluateToConstant[global.type, Any](application)(global)(variables) match {
								case Some(constant) => {
									if (constant.isInstanceOf[Int]) {
										val intConstant = constant.asInstanceOf[Int]
										q"$intConstant"
									}
									else if (constant.isInstanceOf[String]) {
										val stringConstant = constant.asInstanceOf[String]
										q"$stringConstant"
									}
									else stat
								}
								case c => stat
							}
						case _ => stat
					}
					case s :: rest =>
						val newStats: List[Tree] = stats.map(stat => transform(stat) /*evaluateToConstant[global.type, Any](stat)(global)(variables) match {
							case Some(evaluatedConstant) =>
									if (evaluatedConstant.isInstanceOf[Integer]) {
										val intEvaluatedConstant: Int = evaluatedConstant.asInstanceOf[Integer]
										q"$intEvaluatedConstant"
									}
									else if (evaluatedConstant.isInstanceOf[String]) {
										val stringEvaluatedConstant = evaluatedConstant.asInstanceOf[String]
										q"$stringEvaluatedConstant"
									}
									else stat
							case _ => stat
							}*/)
						q"{ ..$newStats }"
					case Nil => q""
				}
			}
		}
	}

	def apply(syntaxTree: Tree, computedResults: List[RuleResult]): BlockConstantPropagatedTree[T] = {
		BlockConstantPropagatedTree(transformer.transform(syntaxTree), transformer.nPropagatedConstants)
	}
}