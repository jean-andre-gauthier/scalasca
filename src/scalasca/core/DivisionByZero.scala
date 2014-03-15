//package scalasca.core
//
//import scala.tools.nsc
//import scala.tools.nsc.Global
//
//class DivisionByZero(global: Global) extends Rule {
//
//	val fatal = false
//	val failureMessage = "definitely division by zero"
//	val name = "Division By Zero"
//	
//	override def eval = {
//			for ( tree @ global.Apply(global.Select(rcvr, global.nme.DIV), List(global.Literal(global.Constant(0)))) <- unit.body;
//					if rcvr.tpe <:< global.definitions.IntClass.tpe)
//			{
//				showRuleFailure(tree.pos)
//			}
//	}
//}