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
package scalasca.core

import scalasca.rules._
import scala.tools.nsc
import nsc.Global
import nsc.Phase
import nsc.plugins.Plugin
import nsc.plugins.PluginComponent
import scala.reflect.runtime.universe._
import scala.reflect.runtime.universe.Flag._
import scala.tools.nsc.Settings


class ScalaSCA(val global: Global) extends Plugin {

	type Global = ScalaSCA.this.global.type
	import global._

	val name = "scalasca"
	val description = "static code analysis checks"
	val components = List[PluginComponent](Component)

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

				val result = new DefaultRule()(global).apply(unit.body)
			}
		}
	}
}
