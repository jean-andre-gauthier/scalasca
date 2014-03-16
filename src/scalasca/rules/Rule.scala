/*******************************************************************************
 * /**
 *  *  _____           _       _____ _____   ___
 *  * /  ___|         | |     /  ___/  __ \ / _ \
 *  * \ `--.  ___ __ _| | __ _\ `--.| /  \// /_\ \
 *  *  `--. \/ __/ _` | |/ _` |`--. \ |    |  _  |
 *  * /\__/ / (_| (_| | | (_| /\__/ / \__/\| | | |
 *  * \____/ \___\__,_|_|\__,_\____/ \____/\_| |_/
 *  *
 *  * Static Code Analyser for Scala.
 *  * (c) 2014, LARA/EPFL, Typesafe
 *  *
 *  * Author: Jean Andre GAUTHIER
 *  * Supervisors: Dr. Viktor KUNCAK, Iulian DRAGOS
 *  *
 *  */
 ******************************************************************************/

package scalasca.rules

import scalasca.core._
import scala.tools.nsc.Global
import scala.tools.nsc.Global._

trait Rule {

	def fatal: Boolean = false
	def failureMessage: String
	def name: String

	def showRuleFailure(position: Global#Position) = {
		if (fatal)
			Console.err.println(position.showError(failureMessage))
		else
			Console.println(position.showError(failureMessage))
	}

	def apply(syntaxTree: Global#Tree, computedResults: List[RuleResult]): (Global#Tree, List[RuleResult])
}
