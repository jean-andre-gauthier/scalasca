package lara.epfl.scalasca.core

abstract class LatticeElement[T](val index: Int, val set: Set[T])

case class Bottom[T](index: Int) extends LatticeElement[T](index, Set[T]())

case class Regular[T](index: Int, set: Set[T]) extends LatticeElement[T](index, set)

case class Top[T](index: Int) extends LatticeElement[T](index, Set[T]())