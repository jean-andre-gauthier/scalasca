package lara.epfl.scalasca.rules

import lara.epfl.scalasca.core._
import scala.tools.nsc._
import scala.collection.mutable._


case class UselessAssignmentPositions(nodes: List[Global#Position]) extends RuleResult {

	override def warning = Warning("Useless Assignment", "Bad practice: variable is re-assigned but not read out subsequently")

	override def toString: String = nodes.foldLeft("")((acc, pos) => acc + "\n" + pos.showError(warning.toString()))

	def toTestString: String = nodes.foldLeft("")((acc, pos) => acc + "\n" + pos.line)
}

/**
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
				case PackageDef(pid, stats) => traverseTrees(stats)
				//Ignores class fields themselves, but analyses their rhs accordingly
				case ClassDef(mods, name, tparams, Template(parents, self, classMembers)) => {
					variablesInScope.addBlockLevel
					classMembers.filter(member => !member.symbol.isMethod).foreach{field => field match {
						case ValDef(mods, name, tpt, expr) => traverse(expr)
						case _ =>
					}}
					traverseTrees(classMembers.filter(member => member.symbol.isMethod))
					variablesInScope.removeBlockLevel
				}
				//Ignores object fields themselves, but analyses their rhs accordingly
				case ModuleDef(mods, name, Template(parents, self, objectMembers)) => {
					variablesInScope.addBlockLevel
					objectMembers.filter(member => !member.symbol.isMethod).foreach{field => field match {
						case ValDef(mods, name, tpt, expr) => traverse(expr)
						case _ =>
					}}
					traverseTrees(objectMembers.filter(member => member.symbol.isMethod))
					variablesInScope.removeBlockLevel
				}
				//Functions, provided they are more than a mere literal
				case functionDefinition @ DefDef(mods, name, tprarams, vparamss, tpt, rhs) => rhs match {
					case Block(_, _) => {
						variablesInScope.addBlockLevel
						traverse(rhs)
						variablesInScope.removeBlockLevel
					}
					case anyOther =>
						traverse(anyOther)
				}
				//Local block => add a block level
				case Block(stats, expr) => {
					variablesInScope.addBlockLevel
					traverseTrees(stats)
					traverse(expr)
					variablesInScope.removeBlockLevel
				}
				case If(cond, thenP, elseP) => {
					traverse(cond)
					variablesInScope.addBlockLevel
					traverse(thenP)
					variablesInScope.removeBlockLevel
					variablesInScope.addBlockLevel
					traverse(elseP)
					variablesInScope.removeBlockLevel
				}
				case varValDef @ ValDef(mods, name, tpt, expr) => {
					expr match {
						case ifBlock @ If(_, _, _) => traverse(ifBlock)
						case _ => traverse(expr)
					}
					variablesInScope.addNonFlagged(name, varValDef)
				}
				case varValAssignment @ Assign(Ident(termname), rhs) => {
					rhs match {
						case ifBlock @ If(_, _, _) => traverse(ifBlock)
						case _ => traverse(rhs)
					}
					variablesInScope.update(termname, Some(false), Some(varValAssignment))
				}
				case Ident(termname @ TermName(_)) =>
					variablesInScope.update(termname, Some(true), None)
				case anyOther =>
					super.traverse(anyOther)
		}
	}

	def apply(syntaxTree: Tree, computedResults: List[RuleResult]): UselessAssignmentPositions = {

		traverser.traverse(syntaxTree)
		UselessAssignmentPositions(for (valDef <- traverser.uselessAssignemnts) yield (valDef.pos))
	}
}