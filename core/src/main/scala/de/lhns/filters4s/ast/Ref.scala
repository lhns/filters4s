package de.lhns.filters4s.ast

case class Ref(name: String, tpeOption: Option[ExprType[_]] = None) extends Expr
