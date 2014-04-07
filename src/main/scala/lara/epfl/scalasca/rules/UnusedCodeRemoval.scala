package lara.epfl.scalasca.rules

import lara.epfl.scalasca.core._
import scala.tools.nsc._

case class UnusedCodeRemovalTree[T <: Global](tree: T#Tree, nRemovedBlocks: Integer) extends RuleResult {

	override def warning = Notice("GEN_UNUSED_CODE_REMOVAL",
			"Removing code that no execution path traverses",
			Console.GREEN + "No unused code found" + Console.RESET,
			GeneralCategory())

	override def toString: String =
		if (nRemovedBlocks > 0)
			warning.formattedWarning + " - " + Console.BLUE + nRemovedBlocks + " unused block(s) removed" + Console.RESET
		else
			warning.formattedDefaultMessage

	override def isSuccess: Boolean = nRemovedBlocks == 0
}

/**
 * GEN_UNUSED_CODE_REMOVAL
 *
 * Removes dead code
 *
 */
class UnusedCodeRemoval[T <: Global](implicit global: T) extends Rule[T]()(global) {

	import global._


	private object transformer extends Transformer {

		private var _nRemovedBlocks = 0
		def nRemovedBlocks = _nRemovedBlocks

		override def transform(tree: Tree): Tree = tree match {
			case q"if($cond) $thenP else $elseP" => cond match {
				case q"true" =>
					_nRemovedBlocks += 1
					thenP
				case q"false" =>
					_nRemovedBlocks += 1
					elseP
				case _ =>
					val newThenP = super.transform(thenP)
					val newElseP = super.transform(elseP)
					q"if($cond) $newThenP else $newElseP"
			}
			case _ =>
				super.transform(tree)
		}
	}

	def apply(syntaxTree: Tree, computedResults: List[RuleResult]): UnusedCodeRemovalTree[T] = {
		UnusedCodeRemovalTree(transformer.transform(syntaxTree), transformer.nRemovedBlocks)
	}
}