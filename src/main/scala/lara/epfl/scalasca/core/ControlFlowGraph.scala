package lara.epfl.scalasca.core

import scala.tools.nsc._

abstract class ControlFlowGraphNode(val node: Option[Global#Tree])
case class MethodDef(val n: Global#Tree) extends ControlFlowGraphNode(Some(n))
case class ValueDef(val n: Global#Tree) extends ControlFlowGraphNode(Some(n))
case class VariableDef(val n: Global#Tree) extends ControlFlowGraphNode(Some(n))
case class AssignNode(val n: Global#Tree) extends ControlFlowGraphNode(Some(n))
case class Label(val n: Global#Tree) extends ControlFlowGraphNode(Some(n))
case class MethodCall(val n: Global#Tree, val targetObject: Set[Global#Tree], val targetMethod: Set[Global#Tree]) extends ControlFlowGraphNode(Some(n))
case class ExprNode(val n: Global#Tree) extends ControlFlowGraphNode(Some(n))
case class EmptyNode() extends ControlFlowGraphNode(None)
case class CatchNode() extends ControlFlowGraphNode(None)
case class ThrowNode(val n: Global#Tree) extends ControlFlowGraphNode(Some(n))

class ControlFlowGraph(
	nodesPrevSucc: Map[ControlFlowGraphNode, (collection.mutable.LinkedHashSet[ControlFlowGraphNode], collection.mutable.LinkedHashSet[ControlFlowGraphNode])] = Map(),
	val entryNodes: collection.mutable.LinkedHashSet[ControlFlowGraphNode] = collection.mutable.LinkedHashSet(),
	val exitNodes: collection.mutable.LinkedHashSet[ControlFlowGraphNode] = collection.mutable.LinkedHashSet()) {

	def withDirectedEdge(from: ControlFlowGraphNode, to: ControlFlowGraphNode): ControlFlowGraph = {
		assert(nodesPrevSucc.contains(from) && nodesPrevSucc.contains(to))
		if (nodesPrevSucc.contains(from) && nodesPrevSucc.contains(to))
			new ControlFlowGraph(
					nodesPrevSucc + (from -> (nodesPrevSucc(from)._1, nodesPrevSucc(from)._2 + to), to -> (nodesPrevSucc(to)._1 + from, nodesPrevSucc(to)._2)),
					entryNodes - to,
					exitNodes - from)
		else
			this
	}

	def withDirectedEdges(fromTo: List[(ControlFlowGraphNode, ControlFlowGraphNode)]): ControlFlowGraph =
		fromTo.foldLeft(this)((acc, ft) => acc.withDirectedEdge(ft._1, ft._2))

	def withDirectedEdges(froms: List[ControlFlowGraphNode], to: ControlFlowGraphNode): ControlFlowGraph =
		withDirectedEdges(froms.map(n => (n, to)))

	def withNode(node: ControlFlowGraphNode): ControlFlowGraph =
		new ControlFlowGraph(nodesPrevSucc + (node -> (collection.mutable.LinkedHashSet(), collection.mutable.LinkedHashSet())), entryNodes + node, exitNodes + node)

	def withNodes(nodes: List[ControlFlowGraphNode]): ControlFlowGraph =
		nodes.foldLeft(this)((acc, n) => acc.withNode(n))

	override def toString(): String =
		"Entry nodes:\n" +
		entryNodes.toList.foldLeft("")((acc, node) =>
			acc + "> " + node + "\n") +
		"Inner nodes:\n" +
		nodesPrevSucc.filter(n => !entryNodes.contains(n._1) && !exitNodes.contains(n._1)).toList.
			foldLeft("")((acc, node) =>
				acc + "    > " + node._1 + "\n" +
				"    Prev:\n" + node._2._1.toList.foldLeft("")((acc, node) => acc + "      " + node + "\n") +
				"    Succ:\n" + node._2._2.toList.foldLeft("")((acc, node) => acc + "      " + node + "\n")) +
		"Exit nodes:\n" +
		exitNodes.toList.foldLeft("")((acc, node) => acc + "> " + node + "\n")
}