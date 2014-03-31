package lara.epfl.scalasca.rules

import scala.tools.nsc._

trait ConstantPropagationEvaluator {

		def evaluateToConstant[T <: Global, U](application: T#Tree)(global: T)(variables: VariablesInScope[global.type, U]): Option[Any] = {

			import global._

			def getAppliedFunction(app: Tree): Option[List[_] => _] = app match {
				case Select(qualifier, name) => evaluateToConstant[global.type, U](qualifier)(global)(variables) match {
					case Some(evaluatedQualifier) => getOperation(evaluatedQualifier, name)
					case _ => None
				}
				case _ => None
			}
			def getEvaluatedArguments(args: List[Tree]): Option[List[Any]] = {
				val evaluatedArguments = args.map(arg => evaluateToConstant[global.type, U](arg)(global)(variables))
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

					//nme.PLUS
					operation match {
						//a + b
						case TermName("$plus") => Some((i: List[_]) => i match {
							case ihead :: irest =>
								val vals = castVals(value, ihead)
								vals._1 + vals._2
							case _ => None
						})
						//a - b
						case TermName("$minus") => Some((i: List[_]) => i match {
							case ihead :: irest =>
								val vals = castVals(value, ihead)
								vals._1 - vals._2
							case _ => None
						})
						//a * b
						case TermName("$mul") => Some((i: List[_]) => i match {
							case ihead :: irest =>
								val vals = castVals(value, ihead)
								vals._1 * vals._2
							case _ => None
						})
						//a / b
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
						//a % b
						case TermName("$percent") => Some((i: List[_]) => i match {
							case ihead :: irest =>
								val vals = castVals(value, ihead)
								vals._1 % vals._2
							case _ => None
						})
						//a < b
						case TermName("$less") => Some((i: List[_]) => i match {
							case ihead :: irest =>
								val vals = castVals(value, ihead)
								vals._1 < vals._2
							case _ => None
						})
						//a <= b
						case TermName("$less$eq") => Some((i: List[_]) => i match {
							case ihead :: irest =>
								val vals = castVals(value, ihead)
								vals._1 <= vals._2
							case _ => None
						})
						//a > b
						case TermName("$greater") => Some((i: List[_]) => i match {
							case ihead :: irest =>
								val vals = castVals(value, ihead)
								vals._1 > vals._2
							case _ => None
						})
						//a >= b
						case TermName("$greater$eq") => Some((i: List[_]) => i match {
							case ihead :: irest =>
								val vals = castVals(value, ihead)
								vals._1 >= vals._2
							case _ => None
						})
						//a | b
						case TermName("$bar") => Some((i: List[_]) => i match {
							case ihead :: irest =>
								value.asInstanceOf[Int] | ihead.asInstanceOf[Int]
							case _ => None
						})
						//a & b
						case TermName("$amp") => Some((i: List[_]) => i match {
							case ihead :: irest =>
								value.asInstanceOf[Int] & ihead.asInstanceOf[Int]
							case _ => None
						})
						//a ^ b
						case TermName("$up") => Some((i: List[_]) => i match {
							case ihead :: irest =>
								value.asInstanceOf[Int] ^ ihead.asInstanceOf[Int]
							case _ => None
						})
						//a == b
						case TermName("$eq$eq") => Some((i: List[_]) => i match {
							case ihead :: irest =>
								val vals = castVals(value, ihead)
								vals._1 == vals._2
							case _ => None
						})
						//a != b
						case TermName("$bang$eq") => Some((i: List[_]) => i match {
							case ihead :: irest =>
								val vals = castVals(value, ihead)
								vals._1 != vals._2
							case _ => None
						})
						case _ => None
					}
				}
				else if (value.isInstanceOf[String]) {
					operation match {
						//s + t
						case TermName("$plus") => Some((i: List[_]) => i match {
							case ihead :: irest => value.asInstanceOf[String] + ihead.asInstanceOf[String]
							case _ => None
						})
						//s.length
						case TermName("length") => Some((i: List[_]) => value.asInstanceOf[String].length)
						case _ => None
					}
				}
				else if (value.isInstanceOf[Boolean]) {
					operation match {
						//a == b
						case TermName("$eq$eq") => Some((i: List[_]) => i match {
							case ihead :: irest =>
								value.asInstanceOf[Boolean] == ihead.asInstanceOf[Boolean]
							case _ => None
						})
						//a != b
						case TermName("$bang$eq") => Some((i: List[_]) => i match {
							case ihead :: irest =>
								value.asInstanceOf[Boolean] != ihead.asInstanceOf[Boolean]
							case _ => None
						})
						//a && b
						case TermName("$amp$amp") => Some((i: List[_]) => i match {
							case ihead :: irest =>
								value.asInstanceOf[Boolean] && ihead.asInstanceOf[Boolean]
							case _ => None
						})
						//a || b
						case TermName("$bar$bar") => Some((i: List[_]) => i match {
							case ihead :: irest =>
								value.asInstanceOf[Boolean] || ihead.asInstanceOf[Boolean]
							case _ => None
						})
						//!a
						case TermName("unary_$bang") => Some((i: List[_]) => i match {
							case ihead :: irest =>
								!value.asInstanceOf[Boolean]
							case _ => None
						})
						case _ => None
					}
				}
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
									Some(appliedFunction(evaluatedArgs))
								}
								case _ => None
							}
						}
						case _ => None
					}
				}
				case Literal(Constant(constant)) => Some(constant)
				case Ident(termname) => variables.findFlagged(termname)
				case _ => None
			}
		}
}