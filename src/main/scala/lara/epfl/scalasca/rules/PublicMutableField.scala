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

case class PublicMutableFieldNodes(nodes: List[Global#Symbol]) extends RuleResult {

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

/**
 * CLS_PUBLIC_MUTABLE_FIELDS
 *
 * Detecs public mutable fields in classes, objects or traits
 *
 */
class PublicMutableField[T <: Global](implicit global: T) extends Rule[T]()(global) {

	import global._

	private object traverser extends Traverser {

		private val _mutableFields = scala.collection.mutable.ListBuffer[Symbol]()
		def publicMutableFields: List[Symbol] = _mutableFields.toList

		override def traverse(tree: Tree): Unit = tree match {
			case q"$mods class $tpname[..$targs] $ctorMods(...$paramss) extends { ..$early } with ..$parents { $self => ..$stats }" =>
				traverseMembers(stats)
			case q"$mods object $tname extends { ..$early } with ..$parents { $self => ..$body }" =>
				traverseMembers(body)
			case q"$mods trait $tpname[..$tparams] extends { ..$earlydefns } with ..$parents { $self => ..$stats }" =>
				traverseMembers(stats)
			case anyOther => traverseTrees(anyOther.children)
		}

		private def traverseMembers(members: List[Tree]): Unit = members.foreach(member => member match {
			case mutableField @ q"$mods var $fileVal: $tpt = $expr" =>
				_mutableFields += mutableField.symbol
				traverse(expr)
			case anyOther =>
				traverse(anyOther)
		})
	}

	def apply(syntaxTree: Tree, computedResults: List[RuleResult]): PublicMutableFieldNodes = {

		traverser.traverse(syntaxTree)
		PublicMutableFieldNodes(traverser.publicMutableFields)
	}
}
