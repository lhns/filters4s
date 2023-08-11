package net.dafttech.filters4s.ast

case class Const[T](value: T, tpe: ExprType[T]) extends Expr {
  override val tpeOption: Option[ExprType[_]] = Some(tpe)
}

object Const {
  object True extends Const(true, ExprType.BoolType) {
    override def toString: String = "True"
  }

  object False extends Const(false, ExprType.BoolType) {
    override def toString: String = "False"
  }
}
