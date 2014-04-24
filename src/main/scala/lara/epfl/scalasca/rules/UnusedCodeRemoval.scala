package lara.epfl.scalasca.rules

import lara.epfl.scalasca.core._
import scala.tools.nsc._

case class UnusedCodeRemovalBlocks(blocksToRemove: Map[Global#Position, Global#Tree]) extends RuleResult with TreeTransformer {

	override def warning = Notice("GEN_UNUSED_CODE_REMOVAL",
			"Removing code that no execution path traverses",
			Console.GREEN + "No unused code found" + Console.RESET,
			GeneralCategory())

	override def toString: String =
		if (blocksToRemove.size > 0)
			warning.formattedWarning + " - " + Console.BLUE + blocksToRemove.size + " unused block(s) removed" + Console.RESET
		else
			warning.formattedDefaultMessage

	override def isSuccess: Boolean = blocksToRemove.size == 0

	override def getTransformation[T <: Global](global: T, tree: T#Tree): T#Tree = {
		import global._

		object transformer extends Transformer {
			override def transform(tree: Tree): Tree = tree match {
				case q"$t" if blocksToRemove.contains(tree.pos) =>
					transform(blocksToRemove(tree.pos.asInstanceOf[Position]).asInstanceOf[Tree])
				case _ =>
					super.transform(tree)
			}
		}
		transformer.transform(tree.asInstanceOf[Tree])
	}
}

case class UnusedCodeRemovalTraversalState(blocksToRemove: Map[Global#Position, Global#Tree]) extends TraversalState

/**
 * GEN_UNUSED_CODE_REMOVAL
 *
 * Removes dead code
 *
 */
class UnusedCodeRemoval[T <: Global](val global: T, inputResults: List[RuleResult] = List()) extends Rule with ConstantPropagationEvaluator {

	type TS = UnusedCodeRemovalTraversalState
	type RR = UnusedCodeRemovalBlocks

	import global._

	override def getDefaultState(): TS = UnusedCodeRemovalTraversalState(Map())

	private val inputSymbolMap = SymbolMapper.getLiteralMapping(inputResults)

	override def step(tree: Global#Tree, state: TS): List[(Option[Position], TS)] = tree match {
//			case q"$mods def $tname[..$tparams](...$paramss): $tpt = $expr" =>
//				goto(expr, state.copy(inMethod = Some(tree.symbol)))
			case q"if($cond) $thenP else $elseP" =>
				val evaluatedCond =
					if (inputSymbolMap.isEmpty)
						cond match {
							case q"true" => Some(true)
							case q"false" => Some(false)
							case _ => None
						}
					else
						evaluateToConstant(cond)(global)(inputSymbolMap) match {
							case Some(value) if value.isInstanceOf[Boolean] =>
								Some(value.asInstanceOf[Boolean])
							case _ => None
						}
				evaluatedCond match {
					case Some(c) if c =>
						goto(thenP, state.copy(blocksToRemove = state.blocksToRemove + (tree.pos -> thenP)))
					case Some(c) if !c =>
						goto(elseP, state.copy(blocksToRemove = state.blocksToRemove + (tree.pos -> elseP)))
					case _ =>
						goto(List(cond, thenP, elseP), state)
				}
//			case q"while ($cond) $expr" => cond match {
//				case q"false" =>
//					goto(Nil, UnusedCodeRemovalTraversalState(state.blocksToRemove + (tree.symbol -> q"")))
//				case _ =>
//					goto(List(cond, expr), state)
//			}
//			case q"do $expr while ($cond)" => cond match {
//				case q"false" =>
//					goto(expr, UnusedCodeRemovalTraversalState(state.blocksToRemove + (tree.symbol -> expr)))
//				case _ =>
//					goto(List(expr,cond), state)
//			}
			case _ =>
				goto(tree.children, state)
	}

	override def getRuleResult(state: TS): RR = UnusedCodeRemovalBlocks(state.blocksToRemove)

	override def apply(syntaxTree: Tree, computedResults: List[RuleResult]): RR = {
		Rule.apply(global)(syntaxTree, List(this)) match {
			case result :: rest => result match {
				case p @ UnusedCodeRemovalBlocks(_) => p
				case _ => UnusedCodeRemovalBlocks(Map())
			}
			case _ => UnusedCodeRemovalBlocks(Map())
		}
	}

	override def mergeStates(s1: TS, s2: TS): TS =
			UnusedCodeRemovalTraversalState(s1.blocksToRemove ++ s2.blocksToRemove)
}