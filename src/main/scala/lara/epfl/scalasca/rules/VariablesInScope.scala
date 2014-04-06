package lara.epfl.scalasca.rules

import scala.tools.nsc._
import scala.collection.mutable._

class VariablesInScope[T <: Global, U](implicit global: T) {

		import global._

		private var variablesInScope = Map[Symbol, (Boolean, Option[U])]()

		def nFlaggedValues = _flaggedValues.size
		def nNonFlaggedValues = _nonFlaggedValues.size
		private var _nonFlaggedValues = List[U]()
		def getNonFlaggedValues: List[U] = _nonFlaggedValues
		private var _flaggedValues = List[U]()
		def getFlaggedValues: List[U] = _flaggedValues

		def findFlagged(symbol: Symbol): Option[U] = {
			variablesInScope.
				get(symbol).
				map(tuple => tuple._2 match {
					case Some(value) => if (tuple._1) tuple._2 else None
					case _ => None
				}).flatten
		}

		def findNonFlagged(symbol: Symbol): Option[U] = {
			variablesInScope.
				get(symbol).
				map(tuple => tuple._2 match {
					case Some(value) => if (!tuple._1) tuple._2 else None
					case _ => None
				}).flatten
		}

		private def addValue(symbol: Symbol, flag: Boolean, value: Option[U]): Unit = {
			variablesInScope += (symbol -> Tuple2(flag, value))
		}

		def addFlagged(symbol: Symbol): Unit = {
			addValue(symbol, true, None)
		}

		def addFlagged(symbol: Symbol, value: U): Unit = {
				addValue(symbol, true, Some(value))
		}

		def addNonFlagged(symbol: Symbol): Unit = {
			addValue(symbol, false, None)
		}

		def addNonFlagged(symbol: Symbol, value: U): Unit = {
				addValue(symbol, false, Some(value))
		}

		//TODO: this method is a piece of crap, redesign it
		def update(symbol: Symbol, flag: Option[Boolean], value: Option[U]): Unit = {
			variablesInScope get symbol match {
				case Some(t) => {
					if (t._2.nonEmpty) {
						if (!t._1 && (!flag.isEmpty && !flag.get))
							_nonFlaggedValues ::= t._2.get
						}
						val current = variablesInScope(symbol)
						val newFlag = flag match {
							case Some(f) => f
							case _ => current._1
						}
						val newValue = value match {
							case v @ Some(_) => v
							case _ => current._2
						}
						variablesInScope(symbol) = (newFlag, newValue)
					}
					case _ =>
			}
		}
}