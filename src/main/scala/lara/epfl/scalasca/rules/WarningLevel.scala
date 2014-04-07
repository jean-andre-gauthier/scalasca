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
package lara.epfl.scalasca.rules

abstract class WarningCategory(category: String) {
	override def toString = category
}

case class NoCategory() extends WarningCategory("")
case class BadPracticeCategory() extends WarningCategory("Bad practice")
case class GeneralCategory() extends WarningCategory("General")
case class FatalCategory() extends WarningCategory("Fatal")

abstract class WarningLevel(originatingRuleName: String, warningMessage: String, defaultMessage: String, category: WarningCategory) {

	private val warningHeader = "ScalaSCA [" + Console.MAGENTA + originatingRuleName + Console.RESET + ", " + category+ "] "
	val formattedWarning = warningHeader + warningMessage
	val formattedDefaultMessage = warningHeader + defaultMessage
}

case class NoWarning() extends WarningLevel("", "", "", NoCategory()) {
	override def toString = ""
}

case class Notice(originatingRuleName: String, noticeMessage: String, defaultMessage: String, category: WarningCategory)
	extends WarningLevel(originatingRuleName, " " + Console.WHITE +  "NOTICE: " + noticeMessage + Console.RESET, defaultMessage, category)
case class Warning(originatingRuleName: String, warningMessage: String, defaultMessage: String, category: WarningCategory)
	extends WarningLevel(originatingRuleName, " " + Console.YELLOW +  "WARNING: " + warningMessage + Console.RESET, defaultMessage, category)
case class SevereWarning(originatingRuleName: String, severeWarningMessage: String, defaultMessage: String, category: WarningCategory)
	extends WarningLevel(originatingRuleName, " " + Console.YELLOW +  "SEVERE WARNING: " + severeWarningMessage + Console.RESET, defaultMessage, category)
case class Fatal(originatingRuleName: String, fatalWarningMessage: String, defaultMessage: String, category: WarningCategory)
	extends WarningLevel(originatingRuleName, " " + Console.RED +  "FATAL: " + fatalWarningMessage + Console.RESET, defaultMessage, category)
