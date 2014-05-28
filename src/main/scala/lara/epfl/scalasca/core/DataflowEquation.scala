package lara.epfl.scalasca.core

import scala.concurrent._
import ExecutionContext.Implicits.global
import scala.concurrent.duration._

case class EquationVariable(val variable: ControlFlowGraphNode, val latticeElement: LatticeElement[ControlFlowGraphNode])

class DataflowEquation(equationFunctions:
	Map[ControlFlowGraphNode,
			(Array[EquationVariable] => Set[ControlFlowGraphNode],
			Set[ControlFlowGraphNode])]) {


	private def boxedInput(): (
			List[EquationVariable],
			Map[EquationVariable, Array[EquationVariable] => EquationVariable],
			Map[EquationVariable, Set[EquationVariable]]) = {

		val newNodes = equationFunctions.zipWithIndex.foldLeft(Map[ControlFlowGraphNode, EquationVariable]())((acc, tuple) =>
			acc + (tuple._1._1 -> EquationVariable(tuple._1._1, Bottom(tuple._2)))
		)

		val newDependencies = equationFunctions.foldLeft(Map[EquationVariable, Set[EquationVariable]]())((acc, tuple) =>
			acc + (newNodes(tuple._1) -> tuple._2._2.map(n => newNodes(n)))
		)

		val newFunctions = equationFunctions.foldLeft(Map[EquationVariable, Array[EquationVariable] => EquationVariable]())((acc, tuple) =>
			acc + (newNodes(tuple._1) -> ((as: Array[EquationVariable]) => EquationVariable(tuple._1, Regular(newNodes(tuple._1).latticeElement.index, tuple._2._1(as))))))

		(newNodes.values.toList, newFunctions, newDependencies)
	}

	private def solutionProcessing(): Map[ControlFlowGraphNode, EquationVariable] = {
			val (boxedVars, functionMap, dependenceMap) = boxedInput()
			val xArray = boxedVars.to[scala.collection.mutable.ArrayBuffer[EquationVariable]]
			val qList = boxedVars.to[scala.collection.mutable.ListBuffer[EquationVariable]]
			while (!qList.isEmpty) {
				val qi = qList.head
				val y = functionMap(qi)(xArray.toArray)
				qList.remove(0)
				if (y != xArray(qi.latticeElement.index)) {
					qList.append(dependenceMap(qi).toSeq:_*)
					xArray(index) = y
				}
			}
			xArray.map(x => (x.variable, x)).toMap
	}

	def solve(iterationTimeLimit: Int = 5000): Option[Map[ControlFlowGraphNode, EquationVariable]] = {
		val solution = Future {
			solutionProcessing()
		}
		try {
			Some(Await.result(solution, iterationTimeLimit.millis))
		}
		catch {
			case e: InterruptedException => None
			case e: TimeoutException => None
		}
	}
}