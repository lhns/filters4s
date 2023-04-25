package net.dafttech.filters4s.ast

case class VarRef(name: String) extends Expr {
  override def tpeOption: Option[ExprType[_]] = None

  final def resolve(scopes: Seq[VarScope]): Option[Expr] =
    scopes.find(_.name == name).map(_.expr)
}
