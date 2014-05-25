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

case class PublicMutableFieldsNodes(nodes: List[Global#Symbol]) extends RuleResult {

	override def warning = Notice("CLS_PUBLIC_MUTABLE_FIELDS",
		"public mutable fields",
		Console.GREEN + "No public mutable fields found" + Console.RESET,
		BadPracticeCategory())

	override def toString: String =
		if (nodes.length > 0)
			nodes.foldLeft("")((acc, sym) => acc + "\n" + sym.pos.showError(warning.formattedWarning))
		else
			warning.formattedDefaultMessage

	override def isSuccess: Boolean = nodes.length == 0
}

case class PublicMutableFieldsTraversalState(inMembers: Boolean, mutableFields: List[Global#Symbol]) extends TraversalState

/**
 * CLS_PUBLIC_MUTABLE_FIELDS
 *
 * Detects public mutable fields in classes, objects or traits
 *
 */
class PublicMutableFields[T <: Global](val global: T) extends ASTRule {

	import global._

	type TS = PublicMutableFieldsTraversalState
	type RR = PublicMutableFieldsNodes

	override val ruleName = "CLS_PUBLIC_MUTABLE_FIELDS"

	override def getDefaultState(): TS = PublicMutableFieldsTraversalState(false, List())

	override def getRuleResult(state: TS): RR = PublicMutableFieldsNodes(state.mutableFields.sortBy(_.pos.point))

	override def mergeStates(s1: TS, s2: TS): TS =
			PublicMutableFieldsTraversalState(s1.inMembers || s2.inMembers, (s1.mutableFields ::: s2.mutableFields).distinct)

	override def step(tree: Global#Tree, state: TS): Map[Option[Int], TS] = tree.asInstanceOf[Tree] match {
			case v @ q"$mods var $fileVal: $tpt = $expr"
				if state.inMembers
					&& !tree.symbol.asInstanceOf[SymbolContextApiImpl].getter.isPrivate
					&& !tree.symbol.asInstanceOf[SymbolContextApiImpl].setter.isPrivate =>
				gotoChildren(expr, PublicMutableFieldsTraversalState(false, tree.symbol :: state.mutableFields))
			case q"$mods class $tpname[..$targs] $ctorMods(...$paramss) extends { ..$early } with ..$parents { $self => ..$stats }" =>
				goto(stats, state.copy(inMembers = true))
			case q"$mods object $tname extends { ..$early } with ..$parents { $self => ..$body }" =>
				goto(body, state.copy(inMembers = true))
			case q"$mods trait $tpname[..$tparams] extends { ..$earlydefns } with ..$parents { $self => ..$stats }" =>
				goto(stats, state.copy(inMembers = true))
			case anyOther =>
				gotoChildren(anyOther, state.copy(inMembers = false))
	}

	override def apply(syntaxTree: Tree, computedResults: List[RuleResult]): RR = {
		ASTRule.apply(global)(syntaxTree, List(this)) match {
			case result :: rest => result match {
				case p @ PublicMutableFieldsNodes(_) => p
				case _ => PublicMutableFieldsNodes(List())
			}
			case _ => PublicMutableFieldsNodes(List())
		}
	}
}
