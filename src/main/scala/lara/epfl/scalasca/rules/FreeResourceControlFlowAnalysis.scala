//package lara.epfl.scalasca.rules
//
//import lara.epfl.scalasca.core._
//import scala.tools.nsc._
//
//case class FreeResourceControlFlowAnalysisResult[T <: Global](nonFreedResources: List[T#Symbol]) extends RuleResult {
//
//	override def warning = Warning("MEM_MISSING_RESOURCE_CLOSING",
//			"Some resources seem not to be closed on all execution paths",
//			Console.GREEN + "No open resources found" + Console.RESET,
//			GeneralCategory())
//
//	override def toString: String =
//		if (nonFreedResources.length > 0)
//			nonFreedResources.foldLeft("")((acc, res) => acc + res.pos.showError(warning.formattedWarning))
//		else
//			warning.formattedDefaultMessage
//
//	override def isSuccess: Boolean = nonFreedResources.length == 0
//}
//
///**
// * MEM_MISSING_RESOURCE_CLOSING
// *
// * Flags any call to openMethodName for which there exists at least one execution path where closeMethodName is not called.
// *
// * TODO Exceptions, allow type specification
// *
// */
//class FreeResourceControlFlowAnalysis[T <: Global](val global: T)(computedResults: List[RuleResult], openMethodName: T#Tree, closeMethodName: T#Ident) extends Rule {
//
//	import global._
//
//		//TODO Handle nested objects
//		//private val closures = Map[Symbol, Symbol]()
//	private var openResources: List[Symbol] = (for (FreeResourceControlFlowAnalysisResult[global.type](open) <- computedResults) yield (open)).flatten
//
//	private abstract class CFGNode {
//		var nextNodes: List[CFGNode]
//	}
//	private case class StartNode() extends CFGNode
//	private case class IFNode() extends CFGNode
//	private case class TryNode() extends CFGNode
//	private case class EndNode() extends CFGNode
//	var currentNode: CFGNode
//
//	def nextNode(tree: Tree): List[Position] = tree match {
//			case file @ q"$mods val $fileVal: $tpt = $method(...$argss)" =>
//				if (openMethodName equalsStructure method) {
//					openResources ::= file.symbol
//					List()
//				}
//				else
//					nextPositions(argss.flatten)
//			case file @ q"$mods val $fileVal: $tpt = $value" => {
//				if (!openResources.get(value.symbol).isEmpty)
//					openResources + (file.symbol -> openResources(value.symbol))
//				else
//					openResources
//			}
//			case q"$instance.$method(...$argss)" => {
//				if (closeMethodName.name == method)
//					if (!openResources.get(instance.symbol).isEmpty)
//						openResources.filter(tuple => tuple._2 == instance.symbol)
//					else
//						openResources
//				else
//					openResources
//			}
//			case q"if ($cond) $thenE else $elseE" => {
//				traverse(thenE, openResources) ++ traverse(elseE, openResources)
//			}
//			case q"if ($cond) $thenE" => {
//				traverse(thenE, openResources)
//			}
//			/*case block @ q"{ ..$stats }" => println("5.5"); stats match {
//				case stat :: Nil => openResources //TODO handle block returns
//				case s :: rest => rest.foldLeft(openResources)((acc, r) => traverse(r, acc))
//				case Nil => openResources
//			}*/
//			case _ => tree.children.foldLeft(openResources)((acc, c) => traverse(c, acc))
//		}
//	}
//	def apply(syntaxTree: Tree, computedResults: List[RuleResult]): FreeResourceControlFlowAnalysisResult[T] = {
//		FreeResourceControlFlowAnalysisResult(traverser.traverse(syntaxTree).values.toList)
//	}
//}