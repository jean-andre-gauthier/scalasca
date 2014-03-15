//package scalasca.core
//
//import scala.tools.nsc
//import nsc.Global
//
//trait Rule {
//	
//	val global: Global
//	val fatal: Boolean
//	val failureMessage: String
//	val name: String
//	def unit: global.CompilationUnit
//
//	def showRuleFailure(position: global.Position) = {
//		if (fatal)
//			unit.error(position, failureMessage)
//		else
//			unit.warning(position, failureMessage)
//	}
//	
//	def eval: Unit
//}