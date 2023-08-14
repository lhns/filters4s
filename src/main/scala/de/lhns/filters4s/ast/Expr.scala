package de.lhns.filters4s.ast

abstract class Expr {
  def tpeOption: Option[ExprType[_]]
}
