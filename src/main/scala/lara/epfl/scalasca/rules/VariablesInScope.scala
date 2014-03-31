package lara.epfl.scalasca.rules

import scala.tools.nsc._
import scala.collection.mutable._

class VariablesInScope[T <: Global, U](implicit global: T) {

		import global._

		private var variablesInScope = List[Map[Name, (Boolean, Option[U])]]()

		def nFlaggedValues = _flaggedValues.size
		def nNonFlaggedValues = _nonFlaggedValues.size
		private var _nonFlaggedValues = List[U]()
		def getNonFlaggedValues: List[U] = _nonFlaggedValues
		private var _flaggedValues = List[U]()
		def getFlaggedValues: List[U] = _flaggedValues

		def findFlagged(termName: Name): Option[U] = {
			variablesInScope.
				find(map => map.get(termName).nonEmpty).
				map(termMap => termMap.get(termName)).
				map(tupleOption => tupleOption match {
					case Some(tuple) => if (tuple._1) tuple._2 else None
					case _ => None
				}).flatten
		}

		def findNonFlagged(termName: Name): Option[U] = {
			variablesInScope.
				find(map => map.get(termName).nonEmpty).
				map(termMap => termMap.get(termName)).
				map(tupleOption => tupleOption match {
					case Some(tuple) => if (!tuple._1) tuple._2 else None
					case _ => None
				}).flatten
		}

		private def addValue(term: Name, flag: Boolean, value: Option[U]): Unit = {
			if (variablesInScope.isEmpty)
				addBlockLevel
			variablesInScope = (variablesInScope.head + (term -> Tuple2(flag, value))) :: variablesInScope.tail
		}

		def addFlagged(term: Name): Unit = {
			addValue(term, true, None)
		}

		def addFlagged(term: Name, value: U): Unit = {
				addValue(term, true, Some(value))
		}

		def addNonFlagged(term: Name): Unit = {
			addValue(term, false, None)
		}

		def addNonFlagged(term: Name, value: U): Unit = {
				addValue(term, false, Some(value))
		}

		def addBlockLevel: Unit =
			variablesInScope = Map[Name, (Boolean, Option[U])]() :: variablesInScope

		//TODO: this method is a piece of crap, redesign it
		def update(term: Name, flag: Option[Boolean], value: Option[U]): Unit = {
			variablesInScope.
				find(map => map contains term).
				foreach(termMap => {
					termMap get term match {
						case Some(t) => {
							if (t._2.nonEmpty) {
								if (!t._1 && (!flag.isEmpty && !flag.get))
									_nonFlaggedValues ::= t._2.get
							}
							val current = termMap(term)
							val newFlag = flag match {
								case Some(f) => f
								case _ => current._1
							}
							val newValue = value match {
								case v @ Some(_) => v
								case _ => current._2
							}
							termMap(term) = (newFlag, newValue)
						}
						case _ =>
					}
				})
		}

		def removeBlockLevel: Unit = {
			variablesInScope = variablesInScope match {
				case innermostBlock :: rest => {
					_flaggedValues :::= innermostBlock.filter(elem => elem._2._1).flatMap(elem => elem._2._2).toList
					_nonFlaggedValues :::= innermostBlock.filter(elem => !elem._2._1).flatMap(elem => elem._2._2).toList
					rest
				}
				case Nil => Nil
			}
		}
	}