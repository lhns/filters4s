package net.dafttech.filters4s.ast

case class Const[T](value: T, tpe: ExprType[T]) extends Expr {

}

object Const {
  object True extends Const(true, ExprType.BoolType)

  object False extends Const(false, ExprType.BoolType)
}
