package net.dafttech.filters4s.ast

case class VarScope(name: String, expr: Expr, body: Expr) extends Expr {
  override def tpeOption: Option[ExprType[_]] = body.tpeOption
}
