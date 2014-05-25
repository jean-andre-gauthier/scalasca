/*******************************************************************************
 *  _____           _       _____ _____   ___
 * /  ___|         | |     /  ___/  __ \ / _ \
 * \ `--.  ___ __ _| | __ _\ `--.| /  \// /_\ \
 *  `--. \/ __/ _` | |/ _` |`--. \ |    |  _  |
 * /\__/ / (_| (_| | | (_| /\__/ / \__/\| | | |
 * \____/ \___\__,_|_|\__,_\____/ \____/\_| |_/
 *
 * Static Code Analyser for Scala.
 * (c) 2014, LARA/EPFL, Typesafe
 *
 * Author: Jean Andre GAUTHIER
 * Supervisors: Dr. Viktor KUNCAK, Iulian DRAGOS
 ******************************************************************************/
package lara.epfl.scalasca.core

import scala.reflect.runtime.universe._
import scala.reflect.runtime.universe.Flag._
import scala.tools.nsc
import scala.tools.nsc.Global
import scala.tools.nsc.Phase
import scala.tools.nsc.Settings
import scala.tools.nsc.plugins.Plugin
import scala.tools.nsc.plugins.PluginComponent
import lara.epfl.scalasca.rules._
import scala.actors.Actor
import java.io.File
import java.net.URLClassLoader


class ScalaSCA(val global: Global) extends Plugin {

	type Global = ScalaSCA.this.global.type
	import global._

	val name = "scalasca"
	val description = "static code analysis checks"
	val components = List[PluginComponent](Component)

	var testRule: String = ""
	var rules = List[Rule]()

	private object Component extends PluginComponent {
		val global: Global = ScalaSCA.this.global
		val runsAfter = List[String]("refchecks");
		val phaseName = ScalaSCA.this.name
		def newPhase(_prev: Phase) = new SCAPhase(_prev)

		class SCAPhase(prev: Phase) extends StdPhase(prev) {
			override def name = ScalaSCA.this.name
			var unit: CompilationUnit = _

			def apply(unit: CompilationUnit) {
				this.unit = unit
				if (rules.isEmpty)
					runRule(testRule)
				else {
					val (astRules, standardRules) = rules.partition(rule => rule.isInstanceOf[ASTRule])
					val standardResults = standardRules.map(rule => rule.apply(unit.body.asInstanceOf[rule.global.Tree], List()))
					val astResults = ASTRule.apply(global)(unit.body, astRules.asInstanceOf[List[ASTRule]])
					new ShowWarnings(global, unit.source.path).apply(unit.body, standardResults ::: astResults)
				}
			}

			/**
			 * The aim here was not to provide a fully fledged rule factory, but rather to facilitate unit tests
			 */
			def runRule(rule: String): Unit = rule match {
				case "blockconstantpropagation" => testBCP()
				case "divisionbyzero" => testDBZ()
				case "doubletripleequals" => testDTE()
				case "emptyfinally" => testEF()
				case "intraproceduralcontrolflowgraph" => testIPCFG()
				case "publicmutablefields" => testPMF()
				case "unusedcoderemoval" => testUCR()
				case "uselessassignment" => testUA()
				case _ =>
					val res = ASTRule.apply(global)(unit.body, List(new PublicMutableFields(global)))
					new ShowWarnings(global, unit.source.path).apply(unit.body, res)
			}

			def testBCP(): Unit =
				ASTRule.apply(global)(unit.body, List(new BlockConstantPropagation(global))) match {
					case BlockConstantPropagatedTree(map) :: rest => println(map.toList.sortBy(_._1.pos.point).map(p => p._1 + "\n" + (p._2 match { case LiteralImage(l) => l }) + "\n").mkString(""))
					case _ =>
				}

			def testDBZ(): Unit =
				ASTRule.apply(global)(unit.body, List(new DivisionByZero(global, ASTRule.apply(global)(unit.body, List(new BlockConstantPropagation(global)))))) match {
					case DivisionByZeroNodes(zeroNodes) :: rest => printNodes(zeroNodes)
					case _ =>
				}

			def testDTE(): Unit =
				ASTRule.apply(global)(unit.body, List(new DoubleTripleEquals[global.type, Actor](global))) match {
					case DoubleTripleEqualsNodes(nodes) :: rest => printSymbols(nodes.asInstanceOf[List[Global#Symbol]])
					case _ =>
				}

			def testEF(): Unit =
				ASTRule.apply(global)(unit.body, List(new EmptyFinally(global))) match {
					case EmptyFinallyNodes(nodes) :: rest => printNodes(nodes)
					case _ =>
				}

			def testIPCFG(): Unit =
				new ShowWarnings(global, unit.source.path).apply(
					unit.body,
					List(new IntraProceduralControlFlowGraphGenerator(global).apply(unit.body, List())))

			def testPMF(): Unit =
				ASTRule.apply(global)(unit.body, List(new PublicMutableFields(global))) match {
					case PublicMutableFieldsNodes(nodes) :: rest => printSymbols(nodes.asInstanceOf[List[Global#Symbol]])
					case _ =>
				}

			def testUCR(): Unit =
				ASTRule.apply(global)(unit.body, List(new UnusedCodeRemoval(global, ASTRule.apply(global)(unit.body, List(new BlockConstantPropagation(global)))))) match {
					case (ucb @ UnusedCodeRemovalBlocks(_)) :: rest => println(ucb.getTransformation(global, unit.body))
					case _ =>
				}

			def testUA(): Unit =
				ASTRule.apply(global)(unit.body, List(new UselessAssignment(global))) match {
					case UselessAssignmentNodes(nodes) :: rest => printNodes(nodes)
					case _ =>
				}

			def printNodes(nodes: List[Global#Position]): Unit =
				println(nodes.map(p => p.lineContent+"\n"+p.lineCaret+"\n"+p.line+" "+p.column+"\n").mkString(""))

			def printSymbols(nodes: List[Global#Symbol]): Unit =
				println(nodes.map(n => n.fullName))
		}
	}

	private def loadRules(f: File): List[Rule] =
		try {
			val c = new URLClassLoader(Array(f.toURI.toURL)).loadClass("ScalaSCAPlugin")
			val loadedRules = c.getMethod("createRules", global.getClass()).invoke(c.newInstance(), global)
			loadedRules.asInstanceOf[List[Rule]]
		}
		catch {
			case e: Exception =>
				e.printStackTrace()
				println("Skipping " + f.getName() + ": not a valid ScalaSCA plugin")
				List()
		}

	override def processOptions(options: List[String], error: String => Unit) {
		for (option <- options) {
			if (option.startsWith("testRule:")) {
				testRule = option.substring(9)
			} else if (option.startsWith("d:") || option.startsWith("f:")) {
				val personalRules =
					if (option.startsWith("d:")) {
						val directory = new File(option.substring(2))
						if (directory.isDirectory())
							directory.listFiles().filter(_.getName.endsWith(".jar")).flatMap(f => loadRules(f)).toList
						else {
							error("Argument for d: is not a directory: " + directory.getName())
							List[Rule]()
						}
					}
					else if (option.startsWith("f:")) {
						val file = new File(option.substring(2))
						if (file.isFile())
							loadRules(file)
						else {
							error("Argument for d: is not a file: " + file.getName())
							List[Rule]()
						}
					}
					else
						List[Rule]()
				rules = personalRules
			}
			else {
				error("Option not understood: " + option)
			}
		}
	}

	override val optionsHelp: Option[String] =
		None
		// Not displaying test option
		//Some("  -P:scalasca:ruleset:r             import the ruleset r")
}
