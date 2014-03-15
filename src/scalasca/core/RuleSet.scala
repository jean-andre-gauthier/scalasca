//package scalasca.core
//
//import scala.tools.nsc.Global
//import scala.reflect.internal.Trees.Tree
//
//class RuleSet(ruleSetLocation: String, global: Global) {
///** Is there a way to do it like that?
// *  
//	val rules: Seq[Rule]
//
//	def loadRules(unit: CompilationUnit): Seq[Rule] = {
//		val ruleNodes = XML.loadFile(ruleSetLocation) \ "rule"
//		val ruleNames = (for (rule <- ruleNodes) yield (rule.attribute("name").toString()))
//		val ucl = ScalaClassLoader.fromURLs(
//					(for (rule <- ruleNodes) yield (rule.attribute("path").toString())).map((a: String) => new URL(a)))
//		ruleNames.map((ruleName: String) => {
//			val ctor = ucl.loadClass(ruleName).getDeclaredConstructor()
//			ctor.newInstance(global, unit).asInstanceOf[Rule]
//		})
//	}
//*/
//	val rules = List[Rule](new DivisionByZero(global))
//	def evalRules = rules.foreach(rule => rule.eval)
//}