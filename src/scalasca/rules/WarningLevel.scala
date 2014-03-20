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
package scalasca.rules

abstract class WarningLevel(originatingRuleName: String, warningMessage: String) {
  override def toString = "ScalaSCA [" + Console.MAGENTA + originatingRuleName + Console.RESET + "] " + warningMessage
}

case class NoWarning() extends WarningLevel("", "") {
	override def toString = ""
}
case class Notice(originatingRuleName: String, noticeMessage: String)
	extends WarningLevel(originatingRuleName, " " + Console.WHITE +  "NOTICE: " + noticeMessage + Console.RESET)
case class Warning(originatingRuleName: String, warningMessage: String)
	extends WarningLevel(originatingRuleName, " " + Console.YELLOW +  "WARNING: " + warningMessage + Console.RESET)
case class SevereWarning(originatingRuleName: String, severeWarningMessage: String)
	extends WarningLevel(originatingRuleName, " " + Console.YELLOW +  "SEVERE WARNING: " + severeWarningMessage + Console.RESET)
case class Fatal(originatingRuleName: String, fatalWarningMessage: String)
	extends WarningLevel(originatingRuleName, " " + Console.RED +  "FATAL: " + fatalWarningMessage + Console.RESET)
