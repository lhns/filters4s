package de.lhns.filters4s.ast

import cats.Invariant
import cats.kernel.Eq
import cats.syntax.invariant._

sealed trait ExprType[T] {
  val name: String

  implicit val eq: Eq[T]

  implicit val ordering: Ordering[T]

  final def unapply[A <: Expr](expr: A): Option[A] =
    Option.when(expr.tpeOption.contains(this))(expr)

  override def toString: String = name

  protected def underying: ExprType[_]
}

object ExprType {
  @inline def apply[T](implicit termType: ExprType[T]): ExprType[T] = termType

  //private implicit def eqFromOrdering[A](implicit ordering: Ordering[A]): Eq[A] = Eq.instance[A](ordering.equiv)

  private val eqAny: Eq[ExprType[_]] = new Eq[ExprType[_]] {
    override def eqv(x: ExprType[_], y: ExprType[_]): Boolean = x.underying == y.underying
  }

  implicit def eq[T]: Eq[ExprType[T]] = eqAny.asInstanceOf[Eq[ExprType[T]]]

  implicit val invariant: Invariant[ExprType] = new Invariant[ExprType] {
    override def imap[A, B](fa: ExprType[A])(f: A => B)(g: B => A): ExprType[B] = new ExprType[B] {
      override val name: String = fa.name

      override implicit val eq: Eq[B] = fa.eq.imap(f)(g)

      override implicit val ordering: Ordering[B] = fa.ordering.imap(f)(g)

      override protected def underying: ExprType[_] = fa
    }
  }

  protected abstract class AbstractExprType[T](
                                                val name: String
                                              )(
                                                implicit
                                                val eq: Eq[T],
                                                val ordering: Ordering[T]
                                              ) extends ExprType[T] {
    override protected def underying: ExprType[_] = this
  }

  implicit case object NullType extends AbstractExprType[Unit]("null")

  implicit case object BoolType extends AbstractExprType[Boolean]("boolean")

  implicit case object StringType extends AbstractExprType[String]("string")

  implicit case object NumberType extends AbstractExprType[BigDecimal]("number")

  implicit val IntType: ExprType[Int] = (NumberType: ExprType[BigDecimal]).imap(_.intValue)(BigDecimal(_))

  implicit val LongType: ExprType[Long] = (NumberType: ExprType[BigDecimal]).imap(_.longValue)(BigDecimal(_))

  implicit val FloatType: ExprType[Float] = (NumberType: ExprType[BigDecimal]).imap(_.floatValue)(BigDecimal(_))

  implicit val DoubleType: ExprType[Double] = (NumberType: ExprType[BigDecimal]).imap(_.doubleValue)(BigDecimal(_))
}
