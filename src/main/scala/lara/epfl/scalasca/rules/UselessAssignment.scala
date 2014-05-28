package lara.epfl.scalasca.rules

import lara.epfl.scalasca.core._
import scala.tools.nsc._


case class UselessAssignmentNodes(uselessAssignments: List[Global#Position]) extends RuleResult {

	override def warning = Warning("ASS_USELESS_ASSIGNMENT",
			"Val/var is being assigned to, but never read subsequently",
			Console.GREEN + "No useless assignments found" + Console.RESET,
			BadPracticeCategory())

	override def toString: String =
		if (uselessAssignments.length > 0)
			uselessAssignments.foldLeft("")((acc, pos) => acc + "\n" + pos.showError(warning.formattedWarning))
		else
			warning.formattedDefaultMessage

	override def isSuccess: Boolean = uselessAssignments.length == 0
}

case class UselessAssignmentTraversalState(currentlyUselessAssignments: Map[Global#Symbol, Global#Position], uselessAssignments: Map[Global#Symbol, List[Global#Position]]) extends TraversalState

/**
 * ASS_USELESS_ASSIGNMENT
 *
 * Searches for values/variables that have an assignment which is not read out subsequently
 *
 * DOES NOT filter out:
 * 		- Fields
 *
 * TODO
 * 		- How to generalise to fields?
 */
class UselessAssignment[T <: Global](val global: T, inputResults: List[RuleResult] = List()) extends ASTRule {

	import global._

	type TS = UselessAssignmentTraversalState
	type RR = UselessAssignmentNodes

	override val ruleName = "ASS_USELESS_ASSIGNMENT"

	override def getDefaultState(): TS = UselessAssignmentTraversalState(Map(), Map())

	override def getRuleResult(state: TS): RR = UselessAssignmentNodes((state.currentlyUselessAssignments.values.toList ::: state.uselessAssignments.values.toList.flatten).distinct.filter(_.isDefined).sortBy(_.point))

	override def mergeStates(s1: TS, s2: TS): TS =
			UselessAssignmentTraversalState(s1.currentlyUselessAssignments, s1.uselessAssignments ++ s2.uselessAssignments)

	override def step(tree: Global#Tree, state: TS): List[(Option[TT], TS)] = { println(tree); tree match {
		case q"package $ref { ..$stats }" =>
			goto(stats, state)
		case q"$mods object $tname extends { ..$early } with ..$parents { $self => ..$body }" =>
			goto(body, state)
		case q"$mods class $tpname[..$targs] $ctorMods(...$paramss) extends { ..$early } with ..$parents { $self => ..$stats }" =>
			goto(stats, state)
		//Ignores trait fields
		case q"$mods trait $tpname[..$tparams] extends { ..$earlydefns } with ..$parents { $self => ..$stats }" =>
			goto(stats, state)
		//Ignores secondary constructors
		case q"$mods def this(...$paramss) = this(..$argss)" =>
			goto(argss, state)
		//Ignores method definitions
		case q"$mods def $tname[..$tparams](...$paramss): $tpt = $expr" =>
			goto(expr, state)
		//Var
		case varDef @ q"$mods var $tname: $tpt = $expr" =>
			goto(expr, state.copy(currentlyUselessAssignments = state.currentlyUselessAssignments + (varDef.symbol -> varDef.pos)))
		//Val
		case valDef @ q"$mods val $tname: $tpt = $expr" =>
			goto(expr, state.copy(currentlyUselessAssignments = state.currentlyUselessAssignments + (valDef.symbol -> valDef.pos)))
		case varAssignment @ q"$lhs = $rhs" =>
			val newState =
				state.currentlyUselessAssignments.get(lhs.symbol) match {
					case Some(s) =>
						state.uselessAssignments.get(lhs.symbol) match {
							case Some(l) =>
								UselessAssignmentTraversalState(state.currentlyUselessAssignments + (lhs.symbol -> varAssignment.pos), state.uselessAssignments + (lhs.symbol -> (s :: l)))
							case None =>
								UselessAssignmentTraversalState(state.currentlyUselessAssignments + (lhs.symbol -> varAssignment.pos), state.uselessAssignments + (lhs.symbol -> List(s)))
						}
					case None =>
						state.copy(currentlyUselessAssignments = state.currentlyUselessAssignments + (lhs.symbol -> varAssignment.pos))
				}
			goto(rhs, newState)
		case anyOther =>
			state.currentlyUselessAssignments.get(anyOther.symbol) match {
				case Some(s) =>
					gotoLeaf(state.copy(currentlyUselessAssignments = state.currentlyUselessAssignments - anyOther.symbol))
				case None =>
					gotoChildren(anyOther, state)
			}
	} }

	override def apply(syntaxTree: Tree): RR = {
		ASTRule.apply(global)(syntaxTree, List(this)) match {
			case result :: rest => result match {
				case u @ UselessAssignmentNodes(_) => u
				case _ => UselessAssignmentNodes(List())
			}
			case _ => UselessAssignmentNodes(List())
		}
	}
}