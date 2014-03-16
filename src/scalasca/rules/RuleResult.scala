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

import scala.reflect.internal.Trees
import scala.reflect.runtime.universe._

abstract class RuleResult
case class SyntaxTree(tree: Trees) extends RuleResult

//TODO Precise specification
case class VariableIntervals() extends RuleResult
case class ControlFlowGraph() extends RuleResult
