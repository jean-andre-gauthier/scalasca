package lara.epfl.scalasca.rules

import lara.epfl.scalasca.core._
import scala.tools.nsc._
import scala.collection.mutable._


case class UselessAssignmentPositions(nodes: List[Global#Position]) extends RuleResult {

	override def warning = Warning("ASS_USELESS_VAR_ASSIGNMENT",
			"Variable is re-assigned but not read out subsequently",
			Console.GREEN + "No useless re-assignments found" + Console.RESET,
			BadPracticeCategory())

	override def toString: String =
		if (nodes.length > 0)
			nodes.foldLeft("")((acc, pos) => acc + "\n" + pos.showError(warning.formattedWarning))
		else
			warning.formattedDefaultMessage

	def toTestString: String = nodes.foldLeft("")((acc, pos) => acc + "\n" + pos.line)

	override def isSuccess: Boolean = nodes.length == 0
}

/**
 * ASS_USELESS_VAR_ASSIGNMENT
 *
 * Searches for variables that have an assignment which is not read out subsequently
 *
 * DOES NOT filter out:
 * 		- Fields
 *
 * TODO
 * 		- How to generalise to fields?
 */
class UselessAssignment[T <: Global](implicit global: T) extends Rule[T]()(global) {

	import global._

	private object traverser extends Traverser {

		private val variablesInScope =
			new VariablesInScope[global.type, Tree]()(global)
		def uselessAssignemnts =
			variablesInScope.getNonFlaggedValues

		override def traverse(tree: Tree): Unit = tree match {
				case q"package $ref { ..$stats }" =>
					traverseTrees(stats)
				//Ignores class fields themselves, but analyses their rhs accordingly
				case q"$mods class $tpname[..$targs] $ctorMods(...$paramss) extends { ..$early } with ..$parents { $self => ..$stats }" => {
					stats.foreach(member => member match {
						case q"$mods def $tname[..$targs](...$paramss): $tpt = $expr" => traverse(member)
						case _ =>})
				}
				//Ignores object fields themselves, but analyses their rhs accordingly
				case q"$mods object $tname extends { ..$early } with ..$parents { $self => ..$body }" => {
					body.foreach(member => member match {
						case q"$mods def $tname[..$targs](...$paramss): $tpt = $expr" => traverse(member)
						case _ =>})
				}
				//Functions, provided they are more than a mere literal
				case functionDefinition @ q"$mods def $tname[..$targs](...$paramss): $tpt = $expr" => expr match {
					case Block(_, _) =>
						traverse(expr)
					case anyOther =>
						traverse(anyOther)
				}
				case q"if ($cond) $thenP else $elseP" => {
					traverse(cond)
					traverse(thenP)
					traverse(elseP)
				}
				case varDef @ q"$mods var $tname: $tpt = $expr" => {
					expr match {
						case ifBlock @ If(_, _, _) => traverse(ifBlock)
						case _ => traverse(expr)
					}
					variablesInScope.addNonFlagged(varDef.symbol, varDef)
				}
				case varAssignment @ q"$lhs = $rhs" => {
					rhs match {
						case ifBlock @ If(_, _, _) => traverse(ifBlock)
						case _ => traverse(rhs)
					}
					variablesInScope.update(lhs.symbol, Some(false), Some(varAssignment))
				}
				case ident @ Ident(TermName(_)) =>
					variablesInScope.update(ident.symbol, Some(true), None)
				//Local block => add a block level
				case block @ q"{ ..$stats }" => stats match {
					case stat :: Nil => ()
					case s :: rest =>
						stats.foreach(stat => traverse(stat))
					case Nil =>
				}
				case anyOther =>
					super.traverse(anyOther)
		}
	}

	def apply(syntaxTree: Tree, computedResults: List[RuleResult]): UselessAssignmentPositions = {
		traverser.traverse(syntaxTree)
		UselessAssignmentPositions(for (valDef <- traverser.uselessAssignemnts) yield (valDef.pos))
	}
}