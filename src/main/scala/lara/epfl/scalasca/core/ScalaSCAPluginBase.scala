package lara.epfl.scalasca.core

import scala.tools.nsc._
import lara.epfl.scalasca.rules._

abstract class ScalaSCAPluginBase {

	def createRules(global: Global): List[Rule]
}