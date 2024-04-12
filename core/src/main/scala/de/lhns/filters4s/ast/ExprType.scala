package de.lhns.filters4s.ast

import cats.Invariant
import cats.kernel.Eq
import cats.syntax.invariant._

sealed trait ExprType[T] {
  val name: String

  implicit val eq: Eq[T]

  implicit val ordering: Ordering[T]

  /*final def unapply[A <: Expr](expr: A): Option[A] =
    Option.when(expr.tpeOption.contains(this))(expr)*/

  final def unapply(const: Const[_]): Option[Const[T]] =
    Option.when(const.tpe == this)(const.asInstanceOf[Const[T]])

  override def toString: String = name

  override def hashCode(): Int = name.hashCode

  override def equals(obj: Any): Boolean = obj match {
    case exprType: ExprType[T] if exprType.name == name => true
    case _ => false
  }
}

object ExprType {
  @inline def apply[T](implicit termType: ExprType[T]): ExprType[T] = termType

  //private implicit def eqFromOrdering[A](implicit ordering: Ordering[A]): Eq[A] = Eq.instance[A](ordering.equiv)

  implicit val invariant: Invariant[ExprType] = new Invariant[ExprType] {
    override def imap[A, B](fa: ExprType[A])(f: A => B)(g: B => A): ExprType[B] = new ExprType[B] {
      override val name: String = fa.name

      override implicit val eq: Eq[B] = fa.eq.imap(f)(g)

      override implicit val ordering: Ordering[B] = fa.ordering.imap(f)(g)
    }
  }

  protected abstract class AbstractExprType[T](
                                                val name: String
                                              )(
                                                implicit
                                                val eq: Eq[T],
                                                val ordering: Ordering[T]
                                              ) extends ExprType[T]

  implicit case object NullType extends AbstractExprType[Unit]("null")

  implicit case object BoolType extends AbstractExprType[Boolean]("boolean")

  implicit case object StringType extends AbstractExprType[String]("string")

  implicit case object NumberType extends AbstractExprType[BigDecimal]("number")

  implicit val IntType: ExprType[Int] = (NumberType: ExprType[BigDecimal]).imap(_.intValue)(BigDecimal(_))

  implicit val LongType: ExprType[Long] = (NumberType: ExprType[BigDecimal]).imap(_.longValue)(BigDecimal(_))

  implicit val FloatType: ExprType[Float] = (NumberType: ExprType[BigDecimal]).imap(_.floatValue)(BigDecimal(_))

  implicit val DoubleType: ExprType[Double] = (NumberType: ExprType[BigDecimal]).imap(_.doubleValue)(BigDecimal(_))
}
