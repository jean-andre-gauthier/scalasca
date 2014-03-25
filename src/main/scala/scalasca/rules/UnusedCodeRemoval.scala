package scalasca.rules

import scalasca.core._
import scala.tools.nsc._

case class UnusedCodeRemovalTree[T <: Global](tree: T#Tree, nRemovedBlocks: Integer) extends RuleResult {

	override def warning = Notice("Unused Code Removal", "Removing code that no execution path traverses")

	override def toString: String = warning.toString + " - " + Console.GREEN + nRemovedBlocks + " unused block(s) removed" + Console.RESET// + "\n" + tree.toString()
}

class UnusedCodeRemoval[T <: Global](implicit global: T) extends Rule[T]()(global) {

	import global._


	private object transformer extends Transformer {

		private var _nRemovedBlocks = 0
		def nRemovedBlocks = _nRemovedBlocks

		override def transform(tree: Tree): Tree = tree match {
			case If(cond, thenP, elseP) => cond match {
				case Literal(Constant(true)) =>
					_nRemovedBlocks += 1
					thenP
				case Literal(Constant(false)) =>
					_nRemovedBlocks += 1
					elseP
				case _ =>
					If(cond, super.transform(thenP), super.transform(elseP))
			}
			case _ =>
				super.transform(tree)
		}
	}

	def apply(syntaxTree: Tree, computedResults: List[RuleResult]): UnusedCodeRemovalTree[T] = {
		UnusedCodeRemovalTree(transformer.transform(syntaxTree), transformer.nRemovedBlocks)
	}
}