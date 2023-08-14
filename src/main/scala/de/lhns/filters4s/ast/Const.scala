package de.lhns.filters4s.ast

import cats.kernel.Eq
import cats.syntax.eq._

import scala.math.Ordering.Implicits._

case class Const[T](value: T, tpe: ExprType[T]) extends Expr {
  override val tpeOption: Option[ExprType[_]] = Some(tpe)

  override def equals(obj: Any): Boolean = obj match {
    case const: Const[T] if const.tpe === this.tpe && const.value == this.value => true
    case _ => false
  }

  def ===(other: Const[T]): Expr = {
    implicit val eq: Eq[T] = tpe.eq
    Const.fromBoolean(tpe === other.tpe && value === other.value)
  }

  def =!=(other: Const[T]): Expr = {
    implicit val eq: Eq[T] = tpe.eq
    Const.fromBoolean(!(tpe === other.tpe && value === other.value))
  }

  def <(other: Const[T]): Expr = {
    implicit val ordering: Ordering[T] = tpe.ordering
    Const.fromBoolean(tpe =!= other.tpe || value < other.value)
  }

  def >(other: Const[T]): Expr = {
    implicit val ordering: Ordering[T] = tpe.ordering
    Const.fromBoolean(tpe =!= other.tpe || value > other.value)
  }

  def <=(other: Const[T]): Expr = {
    implicit val ordering: Ordering[T] = tpe.ordering
    Const.fromBoolean(tpe === other.tpe && value <= other.value)
  }

  def >=(other: Const[T]): Expr = {
    implicit val ordering: Ordering[T] = tpe.ordering
    Const.fromBoolean(tpe === other.tpe && value >= other.value)
  }
}

object Const {
  def apply[T](value: T)(implicit tpe: ExprType[T], dummyImplicit: DummyImplicit): Const[T] = Const(value, tpe)

  object True extends Const(true, ExprType.BoolType) {
    override def toString: String = "True"
  }

  object False extends Const(false, ExprType.BoolType) {
    override def toString: String = "False"
  }

  def fromBoolean(value: Boolean): Const[Boolean] =
    if (value) True else False
}
