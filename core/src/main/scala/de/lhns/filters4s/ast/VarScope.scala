package de.lhns.filters4s.ast

case class VarScope(name: String, expr: Expr, body: Expr) extends Expr {
  override def tpeOption: Option[ExprType[_]] = body.tpeOption

  def withExpr(expr: Expr): VarScope = copy(expr = expr)

  def withBody(body: Expr): VarScope = copy(body = body)
}
