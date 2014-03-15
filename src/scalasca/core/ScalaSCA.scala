package scalasca.core

import scala.tools.nsc
import nsc.Global
import nsc.Phase
import nsc.plugins.Plugin
import nsc.plugins.PluginComponent
import scala.reflect.runtime.universe.Flag._
import scalasca.tests.standalone.TestClassB
import scalasca.tests.standalone.TestClassA
import akka.actor.Actor

class ScalaSCA(val global: Global) extends Plugin {
	import global._

	val name = "scalasca"
	val description = "static code analysis checks"
	val components = List[PluginComponent](Component)

	private object Component extends PluginComponent {
		val global: ScalaSCA.this.global.type = ScalaSCA.this.global
				val runsAfter = List[String]("refchecks");
	val phaseName = ScalaSCA.this.name
			def newPhase(_prev: Phase) = new SCAPhase(_prev)

	class SCAPhase(prev: Phase) extends StdPhase(prev) {
		override def name = ScalaSCA.this.name
		var unit: CompilationUnit = _
		
		/**
		 * How to make this cleaner without causing type errors?
		 */
			trait Rule {
		
				def fatal: Boolean
				def failureMessage: String
				def name: String
			
				def showRuleFailure(position: Position) = {
					if (fatal)
						unit.error(position, failureMessage)
					else
						unit.warning(position, failureMessage)
				}
				
				def eval: Unit
			}
		
			class DivisionByZero extends Rule {
		
				override def fatal = false
				override def failureMessage = "Division by zero"
				override def name = "Division By Zero"
				
				override def eval = {
						for ( tree @ Apply(Select(rcvr, nme.DIV), List(Literal(Constant(0)))) <- unit.body;
								if rcvr.tpe <:< definitions.IntClass.tpe)
						{
							showRuleFailure(tree.pos)
						}
				}
			}
			
			class EmptyFinally extends Rule {
		
				override def fatal = false
				override def failureMessage = "Empty finally block"
				override def name = "Empty Finally"
				
				override def eval = {
						for ( tree @ Try(block, catches, Literal(Constant(()))) <- unit.body)
						{
							showRuleFailure(tree.pos)
						}
				}
			}
			
			/**
			 * This is not clean, how to make it really generic e.g. other methods than === and ==?
			 */
			class DoubleTirpleEquals[A, B](implicit tagA: TypeTag[A], tagB: TypeTag[B]) extends Rule {
		
				override def fatal = false
				override def failureMessage = "Probably you intended to use === instead of =="
				override def name = "Double vs Triple Equals"
				
				override def eval = {
						for ( tree @ Apply(Select(a, nme.EQ), b :: Nil) <- unit.body; if a.tpe <:< tagA.tpe && b.tpe <:< tagB.tpe)
						{
							showRuleFailure(tree.pos)
						}
				}
			}
			
			/**
			 * How to check equality for flags?
			 */
			class PublicMutableFields[A <: Actor] extends Rule {
		
				override def fatal = false
				override def failureMessage = "Bad practice: public mutable fields"
				override def name = "Public Mutable Fields"
				
				override def eval = {
						for ( tree @ ClassDef(_, _, _, Template(_, _, body)) <- unit.body)
						{
							for ( bodyTree @ ValDef(Modifiers(flags, _, _), _, _, _) <- body/*; if flags == MUTABLE*/)
							showRuleFailure(tree.pos)
						}
				}
			}
			
			class RuleSet {
					val rules = List[Rule](
							new DivisionByZero(),
							new EmptyFinally(),
							new DoubleTirpleEquals[TestClassA, TestClassB](),
							new PublicMutableFields()
					)
					def evalRules = rules.foreach(rule => rule.eval)
			}
		
		
		def apply(unit: CompilationUnit) {
			this.unit = unit
			//val rs = new RuleSet("ruleset", global, unit)
			val rs = new RuleSet()
			rs.evalRules
		}
	}
	}


//	override def processOptions(options: List[String], error: String => Unit) {
//		for (option <- options) {
//			if (option.startsWith("level:")) {
//				ruleSet = new RuleSet(option.substring("ruleset:".length))
//			} else {
//				error("Option not understood: " + option)
//			}
//		}
//	}
//
//	override val optionsHelp: Option[String] = Some(
//			"  -P:scalasca:ruleset:r             import the ruleset r")
}