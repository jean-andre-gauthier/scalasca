package lara.epfl.scalasca.rules

import scala.tools.nsc._

abstract class SymbolImage
case class LiteralImage(value: Any) extends SymbolImage
case class IntervalImage() extends SymbolImage

object SymbolMapper {
	def getLiteralMapping(results: List[RuleResult]): Map[Global#Symbol, Any] =
		results.map(_ match {
			case mapper: SymbolMapper => mapper.getMapping()
			case _ => Map()
		}).foldLeft(Map[Global#Symbol, Any]())((acc, cur) => acc ++ cur).
		collect({case (s, LiteralImage(v)) => (s,v)})
}

trait SymbolMapper {

	def getMapping(): Map[Global#Symbol, SymbolImage]
}