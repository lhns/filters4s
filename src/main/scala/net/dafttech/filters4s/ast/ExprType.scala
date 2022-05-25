package net.dafttech.filters4s.ast

import cats.kernel.Eq
import cats.syntax.option._

sealed abstract class ExprType[T](
                                   implicit
                                   val eq: Eq[T],
                                   val ordering: Ordering[T]
                                 ) {
  def unapply[A <: Expr](term: A): Option[A] =
    if (term.tpe == this) term.asInstanceOf[A].some
    else none
}

object ExprType {
  @inline def apply[T](implicit termType: ExprType[T]): ExprType[T] = termType

  private implicit def eqFromOrdering[A](implicit ordering: Ordering[A]): Eq[A] = Eq.instance[A](ordering.equiv)

  implicit object NullType extends ExprType[Unit]

  implicit object BoolType extends ExprType[Boolean]

  implicit object StringType extends ExprType[String]

  implicit object NumberType extends ExprType[BigDecimal]
}
