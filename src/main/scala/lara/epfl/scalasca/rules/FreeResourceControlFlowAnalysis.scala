package lara.epfl.scalasca.rules

import lara.epfl.scalasca.core._
import scala.tools.nsc._
import scala.collection.mutable.LinkedList

case class UnfreedResources(unfreedResources: Set[ControlFlowGraphNode]) extends RuleResult {

	override def warning = Warning("MEM_MISSING_RESOURCE_CLOSING",
			"Some resources seem not to be closed on all execution paths",
			Console.GREEN + "No open resources found" + Console.RESET,
			GeneralCategory())

	override def toString: String =
		if (unfreedResources.size > 0)
			unfreedResources.foldLeft("")((acc, res) => acc + (res.node match {
				case Some(n) => n.pos.showError(warning.formattedWarning)
				case None => ""
			}))
		else
			warning.formattedDefaultMessage

	override def isSuccess: Boolean = unfreedResources.size == 0
}

/**
 * MEM_MISSING_RESOURCE_CLOSING
 *
 * Flags any call to openMethodName for which there exists at least one execution path where closeMethodName is not called.
 *
 * TODO allow type specification
 *
 */
class UnfreedResourcesControlFlowAnalysis[T <: Global](val global: T, computedResults: List[RuleResult], _openMethodName: T#Tree, _closeMethodName: T#Ident) extends Rule {

	import global._

	type RR = UnfreedResources

	override val ruleName: String = "MEM_MISSING_RESOURCE_CLOSING"

	private val openMethodName = _openMethodName.asInstanceOf[global.Tree]
	private val closeMethodName = _closeMethodName.asInstanceOf[global.Tree]

	private val cfgResults: List[IntraProceduralControlFlowGraphMap] =
		computedResults.partition(_.isInstanceOf[IntraProceduralControlFlowGraphMap])._1.asInstanceOf[List[IntraProceduralControlFlowGraphMap]]
	private val cfgOption =
		if (cfgResults.length > 0)
			Some(cfgResults.head)
		else
			None

	private def getEquation(cfg: ControlFlowGraph): DataflowEquation = {

		val resourcesMap = scala.collection.mutable.Map[Global#Symbol, ControlFlowGraphNode]()
		val allNodes = cfg.getAllNodes()
		val equationMap = allNodes.zipWithIndex.foldLeft(Map[ControlFlowGraphNode, (Array[EquationVariable] => Set[ControlFlowGraphNode], Set[ControlFlowGraphNode])]())((acc, tuple) => {

			val (cfgNode, index) = tuple
			val previousNodes = cfg.prevNodesOf(cfgNode).toSet

			acc + (cfgNode match {
				//Resource opening
				case MethodCall(_, targets, m) if m.asInstanceOf[global.Tree].equalsStructure(openMethodName) && targets.size == 1 =>
					val target = targets.toList.head
					resourcesMap += (target -> cfgNode)

					(cfgNode, ((args: Array[EquationVariable]) => {
						val argsPreviousNodes = args.filter(arg => previousNodes.contains(arg.variable))
						if (argsPreviousNodes.isEmpty)
							Set[ControlFlowGraphNode]()
						else
							argsPreviousNodes.foldLeft(Set[ControlFlowGraphNode]())((acc, a) => acc ++ a.latticeElement.set) + cfgNode
					}, previousNodes))
				//Resource closing
				case MethodCall(_, targets, m) if m.asInstanceOf[global.Tree].equalsStructure(closeMethodName) && targets.size == 1 && resourcesMap.contains(targets.toList.head) =>
					val target = targets.toList.head
					val openingNode = resourcesMap(target)
					resourcesMap -= target

					(cfgNode, ((args: Array[EquationVariable]) => {
						val argsPreviousNodes = args.filter(arg => previousNodes.contains(arg.variable))
						if (argsPreviousNodes.isEmpty)
							Set[ControlFlowGraphNode]()
						else
							argsPreviousNodes.foldLeft(Set[ControlFlowGraphNode]())((acc, a) => acc ++ a.latticeElement.set) - cfgNode
					}, previousNodes))
				//All other cases
				case _ =>
					(cfgNode, ((args: Array[EquationVariable]) => {
						val argsPreviousNodes = args.filter(arg => previousNodes.contains(arg.variable))
						if (argsPreviousNodes.isEmpty)
							Set[ControlFlowGraphNode]()
						else
							argsPreviousNodes.foldLeft(Set[ControlFlowGraphNode]())((acc, a) => acc ++ a.latticeElement.set)
					}, previousNodes))
			})
		})
		new DataflowEquation(equationMap)
	}

	def apply(syntaxTree: Tree): RR = {
		val cfg =
			if (cfgOption.isEmpty) {
				val cfgGenerator = new IntraProceduralControlFlowGraphGenerator(global, computedResults)
				cfgGenerator.apply(syntaxTree.asInstanceOf[cfgGenerator.global.Tree])
			}
			else
				cfgOption.get
		UnfreedResources(
			cfg.methodCFGMap.foldLeft(Set[ControlFlowGraphNode]())((acc, c) => {
				val unclosedResourcesMap = getEquation(c._2).solve(10000)
				unclosedResourcesMap match {
					case Some(map) => c._2.exitNodes.toList.flatMap(en => map.get(en) match {
							case Some(n) => n.latticeElement.set.toList
							case None => List()
						}).toSet ++ acc
					case None => acc
				}
			}))
	}
}