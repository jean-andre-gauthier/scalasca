package lara.epfl.scalasca.rules

import lara.epfl.scalasca.core._
import scala.tools.nsc._

case class IntraProceduralControlFlowGraphMap(methodCFGMap: Map[Global#Symbol, ControlFlowGraph]) extends RuleResult {

	override def warning = Warning("GEN_CFG_GENERATOR_INTRA",
		"Generating Control Flow Graph",
		Console.YELLOW + "Unable to generate control flow graph" + Console.RESET,
		FatalCategory())

	override def toString(): String =
		if (methodCFGMap.size > 0)
			methodCFGMap.foldLeft("")((acc, item) => acc + "Method " + item._1.name + ":\n" + item._2.toString())
		else
			warning.formattedDefaultMessage

	override def isSuccess: Boolean = methodCFGMap.size != 0
}

class IntraProceduralControlFlowGraphGenerator[T <: Global](val global: T, inputResults: List[RuleResult] = List()) extends StandardRule {

	import global._

	type RR = IntraProceduralControlFlowGraphMap

	override val ruleName = "GEN_CFG_GENERATOR_INTRA"

	private var funGraphMap = Map[Global#Symbol, ControlFlowGraph]()
	private var labelMap = Map[Symbol, Label]()
	private var unseenLabelsPreviousNodes = Map[Symbol, List[ControlFlowGraphNode]]()

	case class TraversalState(
		currentMethod: Option[Symbol],
		currentCatch: Option[ControlFlowGraphNode],
		previousNodes: List[ControlFlowGraphNode]
	)

	def apply(tree: Tree): RR = {

		def getUpdatedGraph(newNode: ControlFlowGraphNode, newNodePreviousNodes: List[ControlFlowGraphNode], ts: TraversalState): ControlFlowGraph =
			if (ts.currentCatch.isEmpty)
				funGraphMap(ts.currentMethod.get).withNode(newNode).withDirectedEdges(newNodePreviousNodes, newNode)
			else
				funGraphMap(ts.currentMethod.get).withNode(newNode).withDirectedEdges(newNodePreviousNodes, newNode).withDirectedEdge(newNode, ts.currentCatch.get)

		def traverse(tree: Tree, ts: TraversalState): (Option[ControlFlowGraphNode], List[ControlFlowGraphNode]) = tree match {

			case q"$mods object $tname extends { ..$early } with ..$parents { $self => ..$body }" if ts.currentMethod.isEmpty && !body.isEmpty =>
//				println("ObjectDef")
				val firstStatTraversal = traverse(body.head, ts)
				val blockTraversal = body.tail.foldLeft(firstStatTraversal)((prevTraversal, stat) => traverse(stat, ts.copy(previousNodes = prevTraversal._2)))
				(firstStatTraversal._1, blockTraversal._2)

// Quasiquote throws weird match error in some cases?
//			case q"$mods class $tpname[..$targs] $ctorMods(...$paramss) extends { ..$early } with ..$parents { $self => ..$stats }" if ts.currentMethod.isEmpty && !stats.isEmpty =>
//				println("ClassDef")
			case ClassDef(mods, name, tparams, Template(parents, self, stats)) if ts.currentMethod.isEmpty && !stats.isEmpty =>
				val firstStatTraversal = traverse(stats.head, ts)
				val blockTraversal = stats.tail.foldLeft(firstStatTraversal)((prevTraversal, stat) => traverse(stat, ts.copy(previousNodes = prevTraversal._2)))
				(firstStatTraversal._1, blockTraversal._2)


			case q"$mods trait $tpname[..$tparams] extends { ..$earlydefns } with ..$parents { $self => ..$stats }" if ts.currentMethod.isEmpty && !stats.isEmpty =>
//				println("TraitDef")
				val firstStatTraversal = traverse(stats.head, ts)
				val blockTraversal = stats.tail.foldLeft(firstStatTraversal)((prevTraversal, stat) => traverse(stat, ts.copy(previousNodes = prevTraversal._2)))
				(firstStatTraversal._1, blockTraversal._2)


			case m @ q"$mods def $tname[..$tparams](...$paramss): $tpt = $expr" =>
//				println("MethodDef " + tree)
				val currentNode = MethodDef(m)
				funGraphMap += (m.symbol -> new ControlFlowGraph().withNode(currentNode))
				(Some(currentNode), traverse(expr, ts.copy(currentMethod = Some(m.symbol), previousNodes = List(currentNode)))._2)

			case l @ LabelDef(name, params, rhs) if !ts.currentMethod.isEmpty =>
//				println("LabelDef " + tree)
				val graphToModify = funGraphMap(ts.currentMethod.get)
				val newNode = Label(l)
				funGraphMap += ts.currentMethod.get -> getUpdatedGraph(newNode, ts.previousNodes, ts)
				labelMap += (l.symbol -> newNode)
				(Some(newNode), traverse(rhs, ts.copy(previousNodes = List(newNode)))._2)


			case v @ q"$mods val $tname: $tpt = $expr" if !ts.currentMethod.isEmpty =>
//				println("ValDef " + tree)
				val exprTraversal = traverse(expr, ts)
				val newNode = ValueDef(v)
				val (exprEntryNode, exprExitNodes) = (exprTraversal._1.getOrElse(EmptyNode()), exprTraversal._2)
				funGraphMap += ts.currentMethod.get -> getUpdatedGraph(newNode, exprExitNodes, ts)
				(Some(exprEntryNode), List(newNode))


			case v @ q"$mods var $tname: $tpt = $expr" if !ts.currentMethod.isEmpty =>
//				println("VarDef " + tree)
				val exprTraversal = traverse(expr, ts)
				val newNode = VariableDef(v)
				val (exprEntryNode, exprExitNodes) = (exprTraversal._1.getOrElse(EmptyNode()), exprTraversal._2)
				funGraphMap += ts.currentMethod.get -> getUpdatedGraph(newNode, exprExitNodes, ts)
				(Some(exprEntryNode), List(newNode))


			case i @ q"if ($condE) $thenE else $elseE" if !ts.currentMethod.isEmpty =>
//				println("IfElse " + tree)
				val condTraversal = traverse(condE, ts)
				val (condEntryNode, condExitNodes) = (condTraversal._1.getOrElse(EmptyNode()), condTraversal._2)
				val thenTraversal = traverse(thenE, ts.copy(previousNodes = condExitNodes))
				val (thenEntryNode, thenExitNodes) = (thenTraversal._1.getOrElse(EmptyNode()), thenTraversal._2)
				val elseExitNodes = traverse(elseE, ts.copy(previousNodes = condExitNodes))._2
				(Some(condEntryNode), thenExitNodes ::: elseExitNodes)


			case n @ q"new { ..$earlydefns } with ..$parents { $self => ..$stats }" if !ts.currentMethod.isEmpty =>
//				println("New")
				val newNode = NewNode(n)
				funGraphMap += ts.currentMethod.get -> getUpdatedGraph(newNode, ts.previousNodes, ts)
				(Some(newNode), List(newNode))


			case m @ q"$target.$method(...$exprss)" if !ts.currentMethod.isEmpty && exprss.size > 0 =>
//				println("exprss " + tree)
				val flatExprss = exprss.flatten
				val newNode = MethodCall(m, Set(target.symbol), method)
				val exprssTraversal =
					if (!flatExprss.isEmpty) {
						val firstExprssTraversal = traverse(flatExprss.head, ts)
						if (flatExprss.size > 1)
							flatExprss.foldLeft(firstExprssTraversal)((prevTraversal, stat) => traverse(stat, ts.copy(previousNodes = prevTraversal._2)))
						else
							firstExprssTraversal
					}
					else {
						(Some(newNode), List(newNode))
					}
				funGraphMap += ts.currentMethod.get -> getUpdatedGraph(newNode, exprssTraversal._2, ts)
				(exprssTraversal._1, List(newNode))


			case Apply(obj, List()) if !(ts.currentMethod.isEmpty || ts.previousNodes.isEmpty) =>
//				println("Application " + tree)
				if (labelMap.contains(obj.symbol)) {
					funGraphMap += ts.currentMethod.get ->
						funGraphMap(ts.currentMethod.get).withDirectedEdges(ts.previousNodes, labelMap(obj.symbol))
					(None, List(labelMap(obj.symbol)))
				}
				else {
					if (unseenLabelsPreviousNodes.contains(obj.symbol)) {
						unseenLabelsPreviousNodes += obj.symbol -> (unseenLabelsPreviousNodes(obj.symbol) ::: ts.previousNodes)
						(None, List())
					}
					else {
						unseenLabelsPreviousNodes += obj.symbol -> ts.previousNodes
						(None, List())
					}
				}

			//Quasiquote not working
			//case q"try { $tryE } catch { case ..$catchCases } finally { $finallyE }" if !ts.currentMethod.isEmpty =>
			case Try(tryE, catchCases, finallyE) if !ts.currentMethod.isEmpty =>
//				println("trycatch")
				val (tryTraversal, catchExitNodes) =
					if (!catchCases.isEmpty) {
						val catchNode = CatchNode()
						funGraphMap += ts.currentMethod.get -> funGraphMap(ts.currentMethod.get).withNode(catchNode)
						(traverse(tryE, ts.copy(currentCatch = Some(catchNode))),
							catchCases.foldLeft(List[ControlFlowGraphNode]())((acc, catchCase) => acc ::: traverse(catchCase.body, ts.copy(previousNodes = List(catchNode)))._2))
					}
					else
						(traverse(tryE, ts), List())
				val (tryEntryNode, tryExitNodes) = (tryTraversal._1.getOrElse(EmptyNode()), tryTraversal._2)
				val finallyExitNodes =
					traverse(finallyE, ts.copy(previousNodes = tryExitNodes ::: catchExitNodes))._2
				finallyExitNodes match {
					case f :: fs =>
						(Some(tryEntryNode), finallyExitNodes)
					case _ if !catchExitNodes.isEmpty => (Some(tryEntryNode), tryExitNodes ::: catchExitNodes)
					case _ => (Some(tryEntryNode), tryExitNodes)
				}


			case q"return $expr" if !ts.currentMethod.isEmpty =>
//				println("returnExpr")
				val exprTraversal = traverse(expr, ts)
				val (exprEntryNode, exprExitNodes) = (exprTraversal._1.getOrElse(EmptyNode()), exprTraversal._2)
				(Some(exprEntryNode), List())


			case t @ q"throw $expr" if !ts.currentMethod.isEmpty =>
//				println("others in method " + showRaw(tree))
				val throwTraversal = traverse(expr, ts)
				val newNode = ThrowNode(t)
				funGraphMap += ts.currentMethod.get -> getUpdatedGraph(newNode, throwTraversal._2, ts)
				(throwTraversal._1, List())


			case q"{ ..$stats }" if stats.size > 1 =>
//				println("Stats " + showRaw(tree))
				if (stats.forall(s => (s.isInstanceOf[ValDef] && s.asInstanceOf[ValDef].symbol.isCase && s.asInstanceOf[ValDef].symbol.isSynthetic) || s.isInstanceOf[LabelDef])) {
//					println("Case Stats " + showRaw(tree))
					val firstStatTraversal = traverse(stats.head, ts)
					val exitNodes = stats.tail.foldLeft(firstStatTraversal._2)((acc, stat) =>
						if (unseenLabelsPreviousNodes.contains(stat.symbol)) {
							val s = unseenLabelsPreviousNodes(stat.symbol)
							unseenLabelsPreviousNodes -= stat.symbol
							acc ::: traverse(stat, ts.copy(previousNodes = s))._2
						}
						else
							traverse(stat, ts.copy(previousNodes = acc))._2)
					(firstStatTraversal._1, exitNodes)
				}
				else {
					val firstStatTraversal = traverse(stats.head, ts)
					val blockTraversal = stats.tail.foldLeft(firstStatTraversal)((prevTraversal, stat) => traverse(stat, ts.copy(previousNodes = prevTraversal._2)))
					(firstStatTraversal._1, blockTraversal._2)
				}


			case a @ q"$expr1 = $expr2" if !ts.currentMethod.isEmpty =>
//				println("expr1 = expr2")
				val expr2Traversal = traverse(expr2, ts)
				val expr1Traversal = traverse(expr1, ts.copy(previousNodes = expr2Traversal._2))
				val newNode = AssignNode(a)
				funGraphMap += ts.currentMethod.get -> getUpdatedGraph(newNode, expr1Traversal._2, ts)
				(expr2Traversal._1, List(newNode))


			case e if !ts.currentMethod.isEmpty =>
//				println("others in method " + showRaw(tree))
				val newNode = ExprNode(e)
				funGraphMap += ts.currentMethod.get -> getUpdatedGraph(newNode, ts.previousNodes, ts)
				(Some(newNode), List(newNode))


			case e =>
//				println("others not in method")
				val traversalResults = e.children.map(c => traverse(c, ts))
				if (traversalResults.size > 0)
					(traversalResults.head._1, traversalResults.last._2)
				else
					(None, List())
		}

		traverse(tree, TraversalState(None, None, List()))
		IntraProceduralControlFlowGraphMap(funGraphMap)
	}
}