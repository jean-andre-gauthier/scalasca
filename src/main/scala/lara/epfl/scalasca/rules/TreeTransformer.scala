package lara.epfl.scalasca.rules

import scala.tools.nsc._

trait TreeTransformer {

	def getTransformation[T <: Global](global: T, tree: T#Tree): T#Tree
}