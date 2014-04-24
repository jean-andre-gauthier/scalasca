package lara.epfl.scalasca.deprecated

import scala.tools.nsc._
import scala.collection.mutable._

class VariablesInScope[T <: Global, U](global: T) {

		import global._

		private var variablesInScope = Map[Symbol, (Boolean, Option[U])]()

		def nFlaggedValues = getFlaggedValues.size
		def nNonFlaggedValues = getNonFlaggedValues.size
		def getFlaggedValues: List[U] = variablesInScope.filter(v => v._2._1).map(v => v._2._2).flatten.toList
		def getNonFlaggedValues: List[U] = variablesInScope.filter(v => !v._2._1).map(v => v._2._2).flatten.toList

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

		/**
		 * Updates a given symbol's (newFlag if Some currentFlag if None, newValue if Some currentValue if None)
		 */
		def update(symbol: Symbol, flag: Option[Boolean], value: Option[U]): Unit = {
			variablesInScope get symbol match {
				case Some(t) => {
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