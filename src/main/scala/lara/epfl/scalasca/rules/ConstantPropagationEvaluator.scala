package lara.epfl.scalasca.rules

import scala.tools.nsc._

trait ConstantPropagationEvaluator {

		def evaluateToConstant[T <: Global, U](application: T#Tree)(global: T)(variables: VariablesInScope[global.type, U]): Option[Any] = {

			import global._

			def getAppliedFunction(app: Tree): Option[List[_] => _] = app match {
				case q"$qualifier....$name" => evaluateToConstant[global.type, U](qualifier)(global)(variables) match {
					case Some(evaluatedQualifier) => getOperation(evaluatedQualifier, name)
					case _ => None
				}
				case _ => None}
			def getEvaluatedArguments(args: List[Tree]): Option[List[Any]] = {
				val evaluatedArguments = args.map(arg => evaluateToConstant[global.type, U](arg)(global)(variables))
				if (evaluatedArguments.forall(arg => !arg.isEmpty))
					Some(evaluatedArguments.flatten)
				else
					None
			}
			def getOperation(value: Any, operation: Name): Option[List[_] => _] = {
				if (value.isInstanceOf[Integer]) {
					val castValue = value.asInstanceOf[Integer]

					operation match {
						//a + b
						case TermName("$plus") => Some((i: List[_]) => i match {
							case ihead :: irest =>
								if (ihead.isInstanceOf[Integer])
									castValue + ihead.asInstanceOf[Integer]
								else if (ihead.isInstanceOf[Double])
									castValue + ihead.asInstanceOf[Double]
								else if (ihead.isInstanceOf[String])
									castValue + ihead.asInstanceOf[String]
								else
									None
							case _ => None
						})
						//a - b
						case TermName("$minus") => Some((i: List[_]) => i match {
							case ihead :: irest =>
								if (ihead.isInstanceOf[Integer])
									castValue - ihead.asInstanceOf[Integer]
								else if (ihead.isInstanceOf[Double])
									castValue - ihead.asInstanceOf[Double]
								else
									None
							case _ => None
						})
						//a * b
						case TermName("$times") => Some((i: List[_]) => i match {
							case ihead :: irest =>
								if (ihead.isInstanceOf[Integer])
									castValue * ihead.asInstanceOf[Integer]
								else if (ihead.isInstanceOf[Double])
									castValue * ihead.asInstanceOf[Double]
								else
									None
							case _ => None
						})
						//a / b
						case TermName("$div") => Some((i: List[_]) => i match {
							case ihead :: irest =>
								if (ihead.isInstanceOf[Integer]) {
									val intIHead = ihead.asInstanceOf[Integer]
									if (intIHead != 0)
										castValue / intIHead
									else
										None
								}
								else if (ihead.isInstanceOf[Double]) {
									val doubleIHead = ihead.asInstanceOf[Double]
									if (doubleIHead != 0)
										castValue / doubleIHead
									else
										None
								}
								else
									None
							case _ => None
						})
						//a % b
						case TermName("$percent") => Some((i: List[_]) => i match {
							case ihead :: irest =>
								if (ihead.isInstanceOf[Integer]) {
									val intIHead = ihead.asInstanceOf[Integer]
									if (intIHead != 0)
										castValue % intIHead
									else
										None
								}
								else if (ihead.isInstanceOf[Double]) {
									val doubleIHead = ihead.asInstanceOf[Double]
									if (doubleIHead != 0)
										castValue % doubleIHead
									else
										None
								}
								else
									None
							case _ => None
						})
						//a < b
						case TermName("$less") => Some((i: List[_]) => i match {
							case ihead :: irest =>
								if (ihead.isInstanceOf[Integer])
									castValue < ihead.asInstanceOf[Integer]
								else if (ihead.isInstanceOf[Double])
									castValue < ihead.asInstanceOf[Double]
								else
									None
							case _ => None
						})
						//a <= b
						case TermName("$less$eq") => Some((i: List[_]) => i match {
							case ihead :: irest =>
								if (ihead.isInstanceOf[Integer])
									castValue <= ihead.asInstanceOf[Integer]
								else if (ihead.isInstanceOf[Double])
									castValue <= ihead.asInstanceOf[Double]
								else
									None
							case _ => None
						})
						//a > b
						case TermName("$greater") => Some((i: List[_]) => i match {
							case ihead :: irest =>
								if (ihead.isInstanceOf[Integer])
									castValue > ihead.asInstanceOf[Integer]
								else if (ihead.isInstanceOf[Double])
									castValue > ihead.asInstanceOf[Double]
								else
									None
							case _ => None
						})
						//a >= b
						case TermName("$greater$eq") => Some((i: List[_]) => i match {
							case ihead :: irest =>
								if (ihead.isInstanceOf[Integer])
									castValue >= ihead.asInstanceOf[Integer]
								else if (ihead.isInstanceOf[Double])
									castValue >= ihead.asInstanceOf[Double]
								else
									None
							case _ => None
						})
						//a | b
						case TermName("$bar") => Some((i: List[_]) => i match {
							case ihead :: irest =>
								if (ihead.isInstanceOf[Integer])
									castValue | ihead.asInstanceOf[Integer]
								else
									None
							case _ => None
						})
						//a & b
						case TermName("$amp") => Some((i: List[_]) => i match {
							case ihead :: irest =>
								if (ihead.isInstanceOf[Integer])
									castValue & ihead.asInstanceOf[Integer]
								else
									None
							case _ => None
						})
						//a ^ b
						case TermName("$up") => Some((i: List[_]) => i match {
							case ihead :: irest =>
								if (ihead.isInstanceOf[Integer])
									castValue ^ ihead.asInstanceOf[Integer]
								else
									None
							case _ => None
						})
						//a == b
						case TermName("$eq$eq") => Some((i: List[_]) => i match {
							case ihead :: irest =>
								if (ihead.isInstanceOf[Integer])
									castValue == ihead.asInstanceOf[Integer]
								else if (ihead.isInstanceOf[Double])
									castValue == ihead.asInstanceOf[Double]
								else
									false
							case _ => None
						})
						//a != b
						case TermName("$bang$eq") => Some((i: List[_]) => i match {
							case ihead :: irest =>
								if (ihead.isInstanceOf[Integer])
									castValue != ihead.asInstanceOf[Integer]
								else if (ihead.isInstanceOf[Double])
									castValue != ihead.asInstanceOf[Double]
								else
									true
							case _ => None
						})
						case _ => None
					}
				}
				else if (value.isInstanceOf[Double]) {
					val castValue = value.asInstanceOf[Double]

					operation match {
						//a + b
						case TermName("$plus") => Some((i: List[_]) => i match {
							case ihead :: irest =>
								if (ihead.isInstanceOf[Integer])
									castValue + ihead.asInstanceOf[Integer]
								else if (ihead.isInstanceOf[Double])
									castValue + ihead.asInstanceOf[Double]
								else if (ihead.isInstanceOf[String])
									castValue + ihead.asInstanceOf[String]
								else
									None
							case _ => None
						})
						//a - b
						case TermName("$minus") => Some((i: List[_]) => i match {
							case ihead :: irest =>
								if (ihead.isInstanceOf[Integer])
									castValue - ihead.asInstanceOf[Integer]
								else if (ihead.isInstanceOf[Double])
									castValue - ihead.asInstanceOf[Double]
								else
									None
							case _ => None
						})
						//a * b
						case TermName("$times") => Some((i: List[_]) => i match {
							case ihead :: irest =>
								if (ihead.isInstanceOf[Integer])
									castValue * ihead.asInstanceOf[Integer]
								else if (ihead.isInstanceOf[Double])
									castValue * ihead.asInstanceOf[Double]
								else
									None
							case _ => None
						})
						//a / b
						case TermName("$div") => Some((i: List[_]) => i match {
							case ihead :: irest =>
								if (ihead.isInstanceOf[Integer]) {
									val intIHead = ihead.asInstanceOf[Integer]
									if (intIHead != 0)
										castValue / intIHead
									else
										None
								}
								else if (ihead.isInstanceOf[Double]) {
									val doubleIHead = ihead.asInstanceOf[Double]
									if (doubleIHead != 0)
										castValue / doubleIHead
									else
										None
								}
								else
									None
							case _ => None
						})
						//a % b
						case TermName("$percent") => Some((i: List[_]) => i match {
							case ihead :: irest =>
								if (ihead.isInstanceOf[Integer]) {
									val intIHead = ihead.asInstanceOf[Integer]
									if (intIHead != 0)
										castValue % intIHead
									else
										None
								}
								else if (ihead.isInstanceOf[Double]) {
									val doubleIHead = ihead.asInstanceOf[Double]
									if (doubleIHead != 0)
										castValue % doubleIHead
									else
										None
								}
								else
									None
							case _ => None
						})
						//a < b
						case TermName("$less") => Some((i: List[_]) => i match {
							case ihead :: irest =>
								if (ihead.isInstanceOf[Integer])
									castValue < ihead.asInstanceOf[Integer]
								else if (ihead.isInstanceOf[Double])
									castValue < ihead.asInstanceOf[Double]
								else
									None
							case _ => None
						})
						//a <= b
						case TermName("$less$eq") => Some((i: List[_]) => i match {
							case ihead :: irest =>
								if (ihead.isInstanceOf[Integer])
									castValue <= ihead.asInstanceOf[Integer]
								else if (ihead.isInstanceOf[Double])
									castValue <= ihead.asInstanceOf[Double]
								else
									None
							case _ => None
						})
						//a > b
						case TermName("$greater") => Some((i: List[_]) => i match {
							case ihead :: irest =>
								if (ihead.isInstanceOf[Integer])
									castValue > ihead.asInstanceOf[Integer]
								else if (ihead.isInstanceOf[Double])
									castValue > ihead.asInstanceOf[Double]
								else
									None
							case _ => None
						})
						//a >= b
						case TermName("$greater$eq") => Some((i: List[_]) => i match {
							case ihead :: irest =>
								if (ihead.isInstanceOf[Integer])
									castValue >= ihead.asInstanceOf[Integer]
								else if (ihead.isInstanceOf[Double])
									castValue >= ihead.asInstanceOf[Double]
								else
									None
							case _ => None
						})
						//a == b
						case TermName("$eq$eq") => Some((i: List[_]) => i match {
							case ihead :: irest =>
								if (ihead.isInstanceOf[Integer])
									castValue == ihead.asInstanceOf[Integer]
								else if (ihead.isInstanceOf[Double])
									castValue == ihead.asInstanceOf[Double]
								else
									false
							case _ => None
						})
						//a != b
						case TermName("$bang$eq") => Some((i: List[_]) => i match {
							case ihead :: irest =>
								if (ihead.isInstanceOf[Integer])
									castValue != ihead.asInstanceOf[Integer]
								else if (ihead.isInstanceOf[Double])
									castValue != ihead.asInstanceOf[Double]
								else
									true
							case _ => None
						})
						case _ => None
					}
				}
				else if (value.isInstanceOf[String]) {
					val castValue = value.asInstanceOf[String]
					operation match {
						//s + t
						case TermName("$plus") => Some((i: List[_]) => i match {
							case ihead :: irest => castValue + ihead.toString
							case _ => None
						})
						//s.length
						case TermName("length") => Some((i: List[_]) => castValue.length)
						//a == b
						case TermName("$eq$eq") => Some((i: List[_]) => i match {
							case ihead :: irest =>
								if (ihead.isInstanceOf[String])
									castValue == ihead.asInstanceOf[String]
								else
									false
							case _ => None
						})
						//a != b
						case TermName("$bang$eq") => Some((i: List[_]) => i match {
							case ihead :: irest =>
								if (ihead.isInstanceOf[String])
									castValue != ihead.asInstanceOf[String]
								else
									true
							case _ => None
						})
						case _ => None
					}
				}
				else if (value.isInstanceOf[Boolean]) {
					val castValue = value.asInstanceOf[Boolean]
					operation match {
						//a == b
						case TermName("$eq$eq") => Some((i: List[_]) => i match {
							case ihead :: irest =>
								if (ihead.isInstanceOf[Boolean])
									castValue == ihead.asInstanceOf[Boolean]
								else
									false
							case _ => None
						})
						//a != b
						case TermName("$bang$eq") => Some((i: List[_]) => i match {
							case ihead :: irest =>
								if (ihead.isInstanceOf[Boolean])
									castValue != ihead.asInstanceOf[Boolean]
								else
									true
							case _ => None
						})
						//a && b
						case TermName("$amp$amp") => Some((i: List[_]) => i match {
							case ihead :: irest =>
								if (ihead.isInstanceOf[Boolean])
									castValue && ihead.asInstanceOf[Boolean]
								else
									None
							case _ => None
						})
						//a || b
						case TermName("$bar$bar") => Some((i: List[_]) => i match {
							case ihead :: irest =>
								if (ihead.isInstanceOf[Boolean])
									castValue || ihead.asInstanceOf[Boolean]
								else
									None
							case _ => None
						})
						//a ^ b
						case TermName("$up") => Some((i: List[_]) => i match {
							case ihead :: irest =>
								if (ihead.isInstanceOf[Boolean])
									castValue ^ ihead.asInstanceOf[Boolean]
								else
									None
							case _ => None
						})
						//!a
						case TermName("unary_$bang") => Some((i: List[_]) => !value.asInstanceOf[Boolean])
						case _ => None
					}
				}
				else
					None
			}

			application match {
				case q"$fun(...$argss)" => fun match {
					case Literal(Constant(constant)) =>
						Some(constant)
					case ident @ Ident(_) =>
						variables.findFlagged(ident.symbol)
					case _ => getAppliedFunction(fun) match {
						case Some(appliedFunction) => argss match {
							case args :: Nil => getEvaluatedArguments(args) match {
								case Some(evaluatedArgs) => Some(appliedFunction(evaluatedArgs))
								case _ => None
							}
							case _ => Some(appliedFunction(List()))
						}
						case _ => None
					}
				}
				case _ => None
			}
		}
}