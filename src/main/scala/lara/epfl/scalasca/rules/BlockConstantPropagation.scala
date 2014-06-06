package lara.epfl.scalasca.rules

import lara.epfl.scalasca.core._
import scala.tools.nsc._
import reflect.runtime.universe.Transformer
import scala.tools.reflect.ToolBox

case class BlockConstantPropagationMapper(symbolTable: Map[Global#Symbol, SymbolImage]) extends RuleResult with SymbolMapper {

	override def getMapping(): Map[Global#Symbol, SymbolImage] = symbolTable

	override def warning = Notice("GEN_BLOCK_CONST_PROP",
		"Propagating constants inside syntactic blocks (simple operations)",
		Console.GREEN + "No constants to be propagated" + Console.RESET,
		GeneralCategory())

	override def toString: String =
		if (symbolTable.size > 0)
			warning.formattedWarning + Console.BLUE + " " + symbolTable.size + " values evaluated as constants" + Console.RESET
		else
			warning.formattedDefaultMessage

	override def isSuccess: Boolean = symbolTable.size == 0
}

/**
 * GEN_BLOCK_CONST_PROP
 *
 * Considers:
 * 	- Values in simple expression blocks
 */

case class BlockConstantPropagationTraversalState(map: Map[Global#Symbol, Any]) extends TraversalState

class BlockConstantPropagation[T <: Global](val global: T, inputResults: List[RuleResult] = List()) extends ASTRule with ConstantPropagationEvaluator {

	import global._

	type TS = BlockConstantPropagationTraversalState
	type RR = BlockConstantPropagationMapper

	override val ruleName = "GEN_BLOCK_CONST_PROP"

	override def getRuleResult(state: TS): RR = BlockConstantPropagationMapper(state.map.map({
		case (k, v) => (k, LiteralImage(v))
	}))

	override def getDefaultState(): TS = BlockConstantPropagationTraversalState(Map())

	override def mergeStates(s1: TS, s2: TS): TS =
			BlockConstantPropagationTraversalState(s1.map ++ s2.map)

	override def step(tree: Global#Tree, state: TS): List[(Option[TT], TS)] = tree match {
				case q"package $ref { ..$stats }" =>
					goto(stats, state)
				//Ignores class fields
// Quasiquote throws weird match error in some cases?
//				case q"$mods class $tpname[..$targs] $ctorMods(...$paramss) extends { ..$early } with ..$parents { $self => ..$stats }" => {
				case ClassDef(mods, name, tparams, Template(parents, self, stats)) => {
					goto(stats, state, stat => stat match {
						case q"$mods def $tname[..$targs](...$paramss): $tpt = $expr" => true
						case _ => false})
				}
				//Ignores object fields
				case q"$mods object $tname extends { ..$early } with ..$parents { $self => ..$body }" => {
					goto(body, state, member => member match {
						case q"$mods def $tname[..$targs](...$paramss): $tpt = $expr" => true
						case _ => false})
				}
				//Ignores trait fields
				case q"$mods trait $tpname[..$tparams] extends { ..$earlydefns } with ..$parents { $self => ..$stats }" => {
					goto(stats, state, stat => stat match {
						case q"$mods def $tname[..$targs](...$paramss): $tpt = $expr" => true
						case _ => false})
				}
				//Functions, provided they are more than a mere literal
				case functionDefinition @ q"$mods def $tname[..$targs](...$paramss): $tpt = $expr" => expr match {
					case Block(_, _) =>
						goto(expr, state)
					case _ =>
						goto(Nil, state)
				}
				//Regular if
				case q"if ($cond) $thenP else $elseP" =>
					goto(List(cond, thenP, elseP), state)
				case constantVal @ q"$mods val $tname: $tpt = $expr" => expr match {
					//Constant val literal
					case cst @ Literal(Constant(constant)) =>
						gotoLeaf(state.copy(map = state.map + (constantVal.symbol -> constant)))
					case application @ q"$fun(...$args)" =>
						evaluateToConstant(application)(global)(state.map) match {
							case Some(constant) => {
								if (constant.isInstanceOf[Int])
									gotoLeaf(state.copy(map = state.map + (constantVal.symbol -> constant.asInstanceOf[Int])))
								else if (constant.isInstanceOf[Double])
									gotoLeaf(state.copy(map = state.map + (constantVal.symbol -> constant.asInstanceOf[Double])))
								else if (constant.isInstanceOf[Boolean])
									gotoLeaf(state.copy(map = state.map + (constantVal.symbol -> constant.asInstanceOf[Boolean])))
								else if (constant.isInstanceOf[String])
									gotoLeaf(state.copy(map = state.map + (constantVal.symbol -> constant.asInstanceOf[String])))
								else
									gotoLeaf(state)
							}
							case c =>
								gotoLeaf(state)
						}
					case anyOther =>
						goto(anyOther.children, state)
				}
				case block @ q"{ ..$stats }" => stats match {
					case stat :: Nil =>
						goto(stat.children, state)
					case s :: rest =>
						goto(block.children, state)
					case Nil =>
						gotoLeaf(state)
				}
				case anyOther =>
					goto(anyOther.children, state)
	}

	override def apply(syntaxTree: Tree): RR = {
		ASTRule.apply(global)(syntaxTree, List(this)) match {
			case result :: rest => result match {
				case b @ BlockConstantPropagationMapper(_) => b
				case _ => BlockConstantPropagationMapper(Map())
			}
			case _ => BlockConstantPropagationMapper(Map())
		}
	}
}