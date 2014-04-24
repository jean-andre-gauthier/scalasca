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


class ScalaSCA(val global: Global) extends Plugin {

	type Global = ScalaSCA.this.global.type
	import global._

	val name = "scalasca"
	val description = "static code analysis checks"
	val components = List[PluginComponent](Component)
	var testRule: String = ""

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
				runRule(testRule)
			}

			/**
			 * The aim here was not to provide a fully fledged rule factory, but rather to facilitate unit tests
			 */
			def runRule(rule: String): Unit = rule match {
				case "blockconstantpropagation" => testBCP()
				case "divisionbyzero" => testDBZ()
				case "doubletripleequals" => testDTE()
				case "emptyfinally" => testEF()
				case "publicmutablefields" => testPMF()
				case "unusedcoderemoval" => testUCR()
				case "uselessassignment" => testUA()
				case _ =>
					val res = Rule.apply(global)(unit.body, List(new PublicMutableFields(global)))
					new ShowWarnings(global, unit.source.path).apply(unit.body, res)
			}

			def testBCP(): Unit =
				Rule.apply(global)(unit.body, List(new BlockConstantPropagation(global))) match {
					case BlockConstantPropagatedTree(map) :: rest => println(map.toList.sortBy(_._1.pos.point).map(p => p._1 + "\n" + (p._2 match { case LiteralImage(l) => l }) + "\n").mkString(""))
					case _ =>
				}

			def testDBZ(): Unit =
				Rule.apply(global)(unit.body, List(new DivisionByZero(global, Rule.apply(global)(unit.body, List(new BlockConstantPropagation(global)))))) match {
					case DivisionByZeroNodes(zeroNodes) :: rest => printNodes(zeroNodes)
					case _ =>
				}

			def testDTE(): Unit =
				Rule.apply(global)(unit.body, List(new DoubleTripleEquals[global.type, Actor](global))) match {
					case DoubleTripleEqualsNodes(nodes) :: rest => printSymbols(nodes.asInstanceOf[List[Global#Symbol]])
					case _ =>
				}

			def testEF(): Unit =
				Rule.apply(global)(unit.body, List(new EmptyFinally(global))) match {
					case EmptyFinallyNodes(nodes) :: rest => printNodes(nodes)
					case _ =>
				}

			def testPMF(): Unit =
				Rule.apply(global)(unit.body, List(new PublicMutableFields(global))) match {
					case PublicMutableFieldsNodes(nodes) :: rest => printSymbols(nodes.asInstanceOf[List[Global#Symbol]])
					case _ =>
				}

			def testUCR(): Unit =
				Rule.apply(global)(unit.body, List(new UnusedCodeRemoval(global, Rule.apply(global)(unit.body, List(new BlockConstantPropagation(global)))))) match {
					case (ucb @ UnusedCodeRemovalBlocks(_)) :: rest => println(ucb.getTransformation(global, unit.body))
					case _ =>
				}

			def testUA(): Unit =
				Rule.apply(global)(unit.body, List(new UselessAssignment(global))) match {
					case UselessAssignmentNodes(nodes) :: rest => printNodes(nodes)
					case _ =>
				}

			def printNodes(nodes: List[Global#Position]): Unit =
				println(nodes.map(p => p.lineContent+"\n"+p.lineCaret+"\n"+p.line+" "+p.column+"\n").mkString(""))

			def printSymbols(nodes: List[Global#Symbol]): Unit =
				println(nodes.map(n => n.fullName))
		}
	}

	override def processOptions(options: List[String], error: String => Unit) {
		for (option <- options) {
			if (option.startsWith("testRule:")) {
				testRule = option.substring(9)
			} else {
				error("Option not understood: " + option)
			}
		}
	}

	override val optionsHelp: Option[String] =
		None
		// Not displaying test option
		//Some("  -P:scalasca:ruleset:r             import the ruleset r")
}
