package lara.epfl.scalasca.deprecated
//
//import lara.epfl.scalasca.core._
//import scala.tools.nsc._
//import lara.epfl.scalasca.rules.RuleResult
//
//case class IntraProceduralControlFlowGraphMap(map: Map[Global#Symbol, ControlFlowGraph]) extends RuleResult {
//
//	override def warning = Warning("GEN_CFG_GENERATOR_INTRA",
//		"Generating Control Flow Graph",
//		Console.YELLOW + "Unable to generate control flow graph" + Console.RESET,
//		FatalCategory())
//
//	override def toString(): String =
//		if (map.size > 0)
//			map.foldLeft("")((acc, item) => acc + "Method " + item._1.name + ":\n" + item._2.toString())
//		else
//			warning.formattedDefaultMessage
//
//	override def isSuccess: Boolean = map.size != 0
//}
//
//case class IntraProceduralControlFlowGraphGeneratorTraveralState(
//	currentMethod: Option[Global#Symbol],
//	previousNodes: Set[ControlFlowGraphNode],
//	enteringThen: Boolean,
//	thenNodes: Set[ControlFlowGraphNode],
//	enteringElse: Boolean,
//	enteringTry: Boolean,
//	tryNodes: Set[ControlFlowGraphNode],
//	enteringCatch: Boolean,
//	catchNodes: Set[ControlFlowGraphNode],
//	enteringFinally: Boolean,
//	functionToGraph: Map[Global#Symbol, ControlFlowGraph],
//	labels: Map[Global#Symbol, ControlFlowGraphNode])
//
//class IntraProceduralControlFlowGraphGenerator[T <: Global](val global: T) extends ASTRule {
//
//	import global._
//
//	type TS = IntraProceduralControlFlowGraphGeneratorTraveralState
//	type RR = IntraProceduralControlFlowGraphMap
//
//	override val ruleName = "GEN_CFG_GENERATOR_INTRA"
//
//	override def getDefaultState(): TS = IntraProceduralControlFlowGraphGeneratorTraveralState(None, None, Map(), Map())
//
//	override def getRuleResult(state: TS): RR = IntraProceduralControlFlowGraphMap(state.functionToGraph)
//
//	override def mergeStates(s1: TS, s2: TS): TS = {
//		val stateWithLabels = s2.copy(labels = s1.labels ++ s2.labels)
//		if (s1.currentMethod == s2.currentMethod && !s1.currentMethod.isEmpty && !s2.currentMethod.isEmpty) {
//			val currentMethod = s1.currentMethod.get
//			s1.functionToGraph.get(currentMethod) match {
//				case Some(graph1) =>
//					s2.functionToGraph.get(currentMethod) match {
//						case Some(graph2) =>
//							val stateWithNewGraph = stateWithLabels.copy(functionToGraph = s1.functionToGraph ++ s2.functionToGraph + (currentMethod -> graph1))
//							if (s2.enteringElse)
//								stateWithNewGraph.copy(thenNodes = s2.thenNodes ++ graph1.exitNodes)
//							else
//								stateWithNewGraph.copy(previousNodes = s2.previousNodes ++ graph1.exitNodes)
//						case None =>
//							stateWithLabels.copy(functionToGraph = s1.functionToGraph)
//					}
//				case None =>
//					stateWithLabels.copy(functionToGraph = s1.functionToGraph)
//			}
//		}
//		else
//			stateWithLabels.copy(functionToGraph = s1.functionToGraph ++ s2.functionToGraph)
//	}
//
//	override def step(tree: Global#Tree, state: TS): List[(Option[TT], TS)] = tree match {
//		case m @ q"$mods def $tname[..$tparams](...$paramss): $tpt = $expr" =>
//			goto(expr, state.copy(currentMethod = Some(m.symbol), funGraphMap = state.functionToGraph + (m.symbol -> new ControlFlowGraph().withNode(MethodDef(m)))))
//
//
//		case l @ LabelDef(name, params, rhs) if !state.currentMethod.isEmpty =>
//			val graphToModify = state.functionToGraph(state.currentMethod.get)
//			val newNode = Label(l)
//			val newGraph = state.previousNode match {
//				case Some(n) =>
//					graphToModify.withNode(newNode).withDirectedEdge(n, newNode)
//				case None =>
//					graphToModify.withNode(newNode)
//			}
//			goto(rhs, state.copy(labels = state.labels + (l.symbol -> newNode), funGraphMap = state.functionToGraph + (state.currentMethod.get -> newGraph)))
//
//
//		case Apply(obj, List()) if state.labels.contains(obj.symbol) && !(state.currentMethod.isEmpty || state.previousNode.isEmpty) =>
//			val currentMethod = state.currentMethod.get
//			val newGraph = state.functionToGraph(currentMethod).withDirectedEdge(state.previousNode.get, state.labels(obj.symbol))
//			gotoLeaf(state.copy(funGraphMap = state.functionToGraph + (currentMethod -> newGraph)))
//
//
//		case i @ q"if ($cond) $thenE else $elseE" if !state.currentMethod.isEmpty =>
//			val currentGraphMap = state.functionToGraph
//			val graphToModify = currentGraphMap(state.currentMethod.get)
//			val condNode = ExprNode(cond)
//			val graphWithCond =
//				state.previousNodes match {
//					case Nil =>
//						graphToModify.withNode(condNode)
//					case prevNodes =>
//						graphToModify.withNode(condNode).withDirectedEdges(prevNodes.map(n => (n, condNode)))
//				}
//			if (elseE.isEmpty) {
//				val thenNode = ExprNode(thenE)
//				val ifNode = IfThenElse(i, thenNode, None)
//				goto(List((cond, state),
//					(thenE, state.copy(
//					enteringThenBlock = true
//					funGraphMap = currentGraphMap + (state.currentMethod.get -> graphWithCond.withDirectedEdge(condNode, ifNode)),
//					previousNode = Some(thenNode)))))
//			}
//			else {
//				val thenNode = ExprNode(thenE)
//				val elseNode = ExprNode(elseE)
//				val ifNode = IfThenElse(i, thenNode, Some(elseNode))
//				val newGraphMapEntry = (state.currentMethod.get -> graphWithCond.withDirectedEdge(condNode, ifNode).withDirectedEdge(condNode, elseNode))
//				goto(List((cond, state),
//					(thenE, state.copy(
//						enteringThenBlock = true
//						funGraphMap = currentGraphMap + newGraphMapEntry,
//						previousNode = Some(thenNode))),
//					(elseE, state.copy(
//						enteringElseBlock = true
//						funGraphMap = currentGraphMap + newGraphMapEntry,
//						previousNode = Some(elseNode)))))
//			}
//
//
//		case q"try $tryE catch $catchE finally $finallyE" if !state.currentMethod.isEmpty =>
//			val currentGraphMap = state.functionToGraph
//			val graphToModify = currentGraphMap(state.currentMethod.get)
//			val tryNode = Expr(tryE)
//
//			if (catchE.isEmpty) {
//				goto(List(
//					(tryE, state.copy), (finallyE)))
//			}
//			val graphWithTry = graphToModify.withNode(tryNode).withDirectedEdge(state.previousNode, tryNode)
//			if (!catchE.isEmpty) {
//				if (!)
//			}
//
//
//		case q"$mods object $tname extends { ..$early } with ..$parents { $self => ..$body }" if state.currentMethod.isEmpty =>
//			goto(body, state)
//
//
//		case q"$mods class $tpname[..$targs] $ctorMods(...$paramss) extends { ..$early } with ..$parents { $self => ..$stats }" if state.currentMethod.isEmpty =>
//			goto(stats, state)
//
//
//		case q"$mods trait $tpname[..$tparams] extends { ..$earlydefns } with ..$parents { $self => ..$stats }" if state.currentMethod.isEmpty =>
//			goto(stats, state)
//	}
//}