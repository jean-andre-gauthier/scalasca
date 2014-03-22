package scalasca.rules

import scalasca.core._
import scala.tools.nsc._

case class BlockConstantPropagatedTree[T <: Global](tree: T#Tree) extends RuleResult {

	override def warning = Notice("Constant Propagation", "Propagating constants inside syntactic blocks (simple operations)")

	override def toString: String = //Console.GREEN + " succeeded" + Console.RESET
		tree.toString()
}

/**
 * Considers:
 * 	- Values in simple expression blocks
 * 	- Any defined class field, provided a ClassConstantPropagatedMap is provided
 */
class BlockConstantPropagation[T <: Global](implicit global: T) extends Rule[T]()(global) {

	import global._

	//Stores both vals and vars
	object variables {
		private var variablesInScope = List[Map[Name, (Boolean, Option[Any])]]()

		def findConstant(termName: Name): Option[Any] = {
			variablesInScope.
				find(map => map.get(termName).nonEmpty).
				map(termMap => termMap.get(termName)).
				map(tupleOption => tupleOption match {
					case Some(tuple) => if (tuple._1) tuple._2 else None
					case _ => None
				}).flatten
		}

		private def addValue(term: Name, constant: Boolean, propagatedValue: Option[Any]): Unit = {
			if (variablesInScope.isEmpty)
				addBlockLevel
			variablesInScope = (variablesInScope.head + (term -> Tuple2(constant, propagatedValue))) :: variablesInScope.tail
		}

		def addConstant(constantTerm: Name, propagatedValue: Any): Unit =
				addValue(constantTerm, true, Some(propagatedValue))

		def addNonConstant(nonConstant: Name): Unit =
				addValue(nonConstant, false, None)

		def addBlockLevel: Unit =
			variablesInScope = Map[Name, (Boolean, Option[Any])]() :: variablesInScope

		def removeBlockLevel: Unit = {
			variablesInScope = variablesInScope match {
				case innermostBlock :: rest => rest
				case Nil => Nil
			}
		}
	}

	private object transformer extends Transformer {

		override def transform(tree: Tree): Tree = {
			tree match {
				case PackageDef(pid, stats) =>
					PackageDef(pid, super.transformTrees(stats))
				//Ignores class fields
				case ClassDef(mods, name, tparams, Template(parents, self, classMembers)) => {
					variables.addBlockLevel
					val newClassMembers = super.transformTrees(classMembers.filter(member => member.symbol.isMethod))
					ClassDef(mods, name, tparams, Template(parents, self, newClassMembers))
				}
				//Ignores object fields
				case ModuleDef(mods, name, Template(parents, self, objectMembers)) => {
					val newObjectMembers = super.transformTrees(objectMembers.filter(tree => tree.symbol.isMethod))
					ModuleDef(mods, name, Template(parents, self, newObjectMembers))
				}
				//Functions, provided they are more than a mere literal
				case functionDefinition @ DefDef(mods, name, tprarams, vparamss, tpt, rhs) => rhs match {
					case Block(_, _) => {
						variables.addBlockLevel
						val toReturn = DefDef(mods, name, tprarams, vparamss, tpt, super.transform(rhs))
						variables.removeBlockLevel
						toReturn
					}
					case _ =>
						functionDefinition
				}
				//Local block => add a block level
				case Block(stats, expr) => {
					variables.addBlockLevel
					val newStats = super.transformTrees(stats)
					val newExpr = super.transform(expr)
					variables.removeBlockLevel
					Block(newStats, newExpr)
				}
				//Constant val literal
				case constantVal @ ValDef(mods, name, tpt, Literal(Constant(value))) =>
					if (!mods.isMutable && !constantVal.symbol.isParameter)
						variables.addConstant(name, value)
					constantVal
				//Constant val built from other constants
				case value @ ValDef(mods, name, tpt, application @ Apply(fun , args)) =>
					evaluateToConstant(application) match {
						case Some(constant) => ValDef(mods, name, tpt, Literal(Constant(constant)))
						case _ => value
					}
				//Regular if
				case If(cond, thenP, elseP) =>
					If(cond, super.transform(thenP), super.transform(elseP))
				case anyOther => anyOther
			}
		}

		private def evaluateToConstant(application: Tree): Option[Any] = {
			def getAppliedFunction(app: Tree): Option[List[_] => _] = app match {
				case Select(qualifier, name) => evaluateToConstant(qualifier) match {
					case Some(evaluatedQualifier) => getOperation(evaluatedQualifier, name)
					case _ => None
				}
				case _ => None
			}
			def getEvaluatedArguments(args: List[Tree]): Option[List[Any]] = {
				val evaluatedArguments = args.map(arg => evaluateToConstant(arg))
				if (evaluatedArguments.forall(arg => !arg.isEmpty))
					Some(evaluatedArguments.flatten)
				else
					None
			}
			def getOperation(value: Any, operation: Name): Option[List[_] => _] = {
				if (value.isInstanceOf[Int] || value.isInstanceOf[Double]) {
					def castVals(a: Any, b: Any) =
						(if (a.isInstanceOf[Double]) a.asInstanceOf[Double] else a.asInstanceOf[Int],
						if (b.isInstanceOf[Double]) b.asInstanceOf[Double] else b.asInstanceOf[Int])

					operation match {
						case TermName("$plus") => Some((i: List[_]) => i match {
							case ihead :: irest =>
								val vals = castVals(value, ihead)
								vals._1 + vals._2
							case _ => None
						})
						case TermName("$minus") => Some((i: List[_]) => i match {
							case ihead :: irest =>
								val vals = castVals(value, ihead)
								vals._1 - vals._2
							case _ => None
						})
						case TermName("$mul") => Some((i: List[_]) => i match {
							case ihead :: irest =>
								val vals = castVals(value, ihead)
								vals._1 * vals._2
							case _ => None
						})
						case TermName("$div") => Some((i: List[_]) => i match {
							case ihead :: irest =>
								val vals = castVals(value, ihead)
								val toReturn = vals._1 / vals._2
								if (value.isInstanceOf[Int] && ihead.isInstanceOf[Int])
									toReturn.floor
								else
									toReturn
							case _ => None
						})
						case _ => None
					}
				}
				else if (value.isInstanceOf[String]) {
					operation match {
						case TermName("$plus") => Some((i: List[_]) => i match {
							case ihead :: irest => value.asInstanceOf[String] + ihead.asInstanceOf[String]
							case _ => None
						})
						case TermName("length") => Some((i: List[_]) => value.asInstanceOf[String].length)
						case _ => None
					}
				}
//				else if (value.isInstanceOf[Boolean]) {
//					operation match {
//						case TermName("$plus") => Some((i: List[_]) => i match {
//							case ihead :: irest => value.asInstanceOf[String] + ihead.asInstanceOf[String]
//							case _ => None
//						})
//						case TermName("length") => Some((i: List[_]) => value.asInstanceOf[String].length)
//						case _ => None
//					}
//				}
				else
					None
			}

			application match {
				case Apply(fun, args) => {
					getAppliedFunction(fun) match {
						case Some(appliedFunction) => {
							val evaluatedArgsOption = getEvaluatedArguments(args)
							evaluatedArgsOption match {
								case Some(evaluatedArgs) => {
									println(">>>>>>>> " + evaluatedArgs)
									Some(appliedFunction(evaluatedArgs))
								}
								case _ => None
							}
						}
						case _ => None
					}
				}
				case Literal(Constant(constant)) => Some(constant)
				case Ident(termname) => variables.findConstant(termname)
				case _ => None
			}
		}
	}

	def apply(syntaxTree: Tree, computedResults: List[RuleResult]): BlockConstantPropagatedTree[T] = {
		BlockConstantPropagatedTree(transformer.transform(syntaxTree))
	}
}