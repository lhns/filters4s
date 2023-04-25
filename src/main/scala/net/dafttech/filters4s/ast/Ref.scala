package net.dafttech.filters4s.ast

case class Ref(name: String) extends Expr {
  override def tpeOption: Option[ExprType[_]] = None
}
