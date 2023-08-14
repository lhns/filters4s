package de.lhns.filters4s.ast

import cats.Monad
import cats.instances.seq._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.traverse._

sealed abstract class Op extends Expr {
  type Self <: Op

  def transformOperandsF[F[_] : Monad](f: Expr => F[Expr]): F[Self]

  override val tpeOption: Option[ExprType[_]] = Some(ExprType.BoolType)
}

object Op {
  implicit class ExprOps[T](val self: Expr) extends AnyVal {
    def ===(expr: Expr): Eq = Eq(self, expr)

    def =!=(expr: Expr): Not = Not(Eq(self, expr))

    def <(expr: Expr): Lt = Lt(self, expr)

    def >(expr: Expr): Gt = Gt(self, expr)

    def <=(expr: Expr): Not = Not(Gt(self, expr))

    def >=(expr: Expr): Not = Not(Lt(self, expr))

    def in(exprs: Seq[Expr]): In = In(self, exprs)

    def unary_! : Not = Not(self)

    def &&(expr: Expr): And = And(self, expr)

    def ||(expr: Expr): Or = Or(self, expr)
  }

  case class Eq(a: Expr, b: Expr) extends Op {
    override type Self = Eq

    override def transformOperandsF[F[_] : Monad](f: Expr => F[Expr]): F[Self] =
      for {
        a2 <- f(a)
        b2 <- f(b)
      } yield
        Eq(a2, b2)
  }

  case class Lt(a: Expr, b: Expr) extends Op {
    override type Self = Lt

    override def transformOperandsF[F[_] : Monad](f: Expr => F[Expr]): F[Self] =
      for {
        a2 <- f(a)
        b2 <- f(b)
      } yield
        Lt(a2, b2)
  }

  case class Gt(a: Expr, b: Expr) extends Op {
    override type Self = Gt

    override def transformOperandsF[F[_] : Monad](f: Expr => F[Expr]): F[Self] =
      for {
        a2 <- f(a)
        b2 <- f(b)
      } yield
        Gt(a2, b2)
  }

  case class In(a: Expr, bSeq: Seq[Expr]) extends Op {
    override type Self = In

    override def transformOperandsF[F[_] : Monad](f: Expr => F[Expr]): F[Self] =
      for {
        a2 <- f(a)
        bSeq2 <- bSeq.map(f).sequence
      } yield
        In(a2, bSeq2)
  }

  case class Not(expr: Expr) extends Op {
    override type Self = Not

    override def transformOperandsF[F[_] : Monad](f: Expr => F[Expr]): F[Self] =
      for {
        expr2 <- f(expr)
      } yield
        Not(expr2)
  }

  case class And(exprs: Seq[Expr]) extends Op {
    override type Self = And

    override def toString: String = productPrefix + exprs.mkString("(", ",", ")")

    override def transformOperandsF[F[_] : Monad](f: Expr => F[Expr]): F[Self] =
      for {
        exprs2 <- exprs.map(f).sequence
      } yield
        And(exprs2)
  }

  object And {
    def apply(expr0: Expr, exprs: Expr*): And = And(expr0 +: exprs)
  }

  case class Or(exprs: Seq[Expr]) extends Op {
    override type Self = Or

    override def toString: String = productPrefix + exprs.mkString("(", ",", ")")

    override def transformOperandsF[F[_] : Monad](f: Expr => F[Expr]): F[Self] =
      for {
        exprs2 <- exprs.map(f).sequence
      } yield
        Or(exprs2)
  }

  object Or {
    def apply(expr0: Expr, exprs: Expr*): Or = Or(expr0 +: exprs)
  }
}
