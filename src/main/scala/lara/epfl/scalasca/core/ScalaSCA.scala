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
				case "blockconstantpropagation" =>
					val constProp = new BlockConstantPropagation()(global).apply(unit.body, List())
					println(constProp.tree)
				case _ =>
					new DefaultRule()(global, unit.source.path).apply(unit.body)
			}
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
