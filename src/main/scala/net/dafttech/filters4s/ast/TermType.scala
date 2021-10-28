package net.dafttech.filters4s.ast

import cats.kernel.Eq
import cats.syntax.option._

sealed abstract class TermType[T](
                                   implicit
                                   val eq: Eq[T],
                                   val ordering: Ordering[T]
                                 ) {
  def unapply[F[_] <: Term[_]](term: F[_]): Option[F[T]] =
    if (term.termType == this) term.asInstanceOf[F[T]].some
    else none
}

object TermType {
  @inline def apply[T](implicit termType: TermType[T]): TermType[T] = termType

  private implicit def eqFromOrdering[A](implicit ordering: Ordering[A]): Eq[A] = Eq.instance[A](ordering.equiv)

  implicit object NullType extends TermType[Unit]

  implicit object BoolType extends TermType[Boolean]

  implicit object StringType extends TermType[String]

  implicit object NumberType extends TermType[BigDecimal]
}
