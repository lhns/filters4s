package net.dafttech.filters4s.ast

import cats.Monad
import cats.instances.seq._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.traverse._
import net.dafttech.filters4s.ast.Op._

sealed abstract class Op extends Expr {
  type Self <: Op

  def foldOp[C](
                 eq: Eq => C,
                 le: Le => C,
                 leq: LEq => C,
                 not: Not => C,
                 and: And => C,
                 or: Or => C,
               ): C

  def transformOperandsF[F[_] : Monad](f: Expr => F[Expr]): F[Self]

  override val tpeOption: Option[ExprType[_]] = Some(ExprType.BoolType)
}

object Op {
  implicit class ExprOps[T](val self: Expr) extends AnyVal {
    def ===(expr: Expr): Eq = Eq(self, expr)

    def =!=(expr: Expr): Not = Not(Eq(self, expr))

    def <(expr: Expr): Le = Le(self, expr)

    def >(expr: Expr): Not = Not(LEq(self, expr))

    def <=(expr: Expr): LEq = LEq(self, expr)

    def >=(expr: Expr): Not = Not(Le(self, expr))

    def in(exprs: Seq[Expr]): In = In(self, exprs)

    def unary_! : Not = Not(self)

    def &&(expr: Expr): And = And(self, expr)

    def ||(expr: Expr): Or = Or(self, expr)
  }

  case class Eq(a: Expr, b: Expr) extends Op {
    override type Self = Eq

    override def foldOp[C](
                            eq: Eq => C,
                            le: Le => C,
                            leq: LEq => C,
                            not: Not => C,
                            and: And => C,
                            or: Or => C,
                          ): C = eq(this)

    override def transformOperandsF[F[_] : Monad](f: Expr => F[Expr]): F[Self] =
      for {
        a2 <- f(a)
        b2 <- f(b)
      } yield
        Eq(a2, b2)
  }

  case class Le(a: Expr, b: Expr) extends Op {
    override type Self = Le

    override def foldOp[C](
                            eq: Eq => C,
                            le: Le => C,
                            leq: LEq => C,
                            not: Not => C,
                            and: And => C,
                            or: Or => C,
                          ): C = le(this)

    override def transformOperandsF[F[_] : Monad](f: Expr => F[Expr]): F[Self] =
      for {
        a2 <- f(a)
        b2 <- f(b)
      } yield
        Le(a2, b2)
  }

  case class LEq(a: Expr, b: Expr) extends Op {
    override type Self = LEq

    override def foldOp[C](
                            eq: Eq => C,
                            le: Le => C,
                            leq: LEq => C,
                            not: Not => C,
                            and: And => C,
                            or: Or => C,
                          ): C = leq(this)

    override def transformOperandsF[F[_] : Monad](f: Expr => F[Expr]): F[Self] =
      for {
        a2 <- f(a)
        b2 <- f(b)
      } yield
        LEq(a2, b2)
  }

  case class In(a: Expr, bSeq: Seq[Expr]) extends Op {
    override type Self = In

    override def foldOp[C](
                            eq: Eq => C,
                            le: Le => C,
                            leq: LEq => C,
                            not: Not => C,
                            and: And => C,
                            or: Or => C,
                          ): C = or(Or(bSeq.map(b => a === b)))

    override def transformOperandsF[F[_] : Monad](f: Expr => F[Expr]): F[Self] =
      for {
        a2 <- f(a)
        bSeq2 <- bSeq.map(f).sequence
      } yield
        In(a2, bSeq2)
  }

  case class Not(expr: Expr) extends Op {
    override type Self = Not

    override def foldOp[C](
                            eq: Eq => C,
                            le: Le => C,
                            leq: LEq => C,
                            not: Not => C,
                            and: And => C,
                            or: Or => C,
                          ): C = not(this)

    override def transformOperandsF[F[_] : Monad](f: Expr => F[Expr]): F[Self] =
      for {
        expr2 <- f(expr)
      } yield
        Not(expr2)
  }

  case class And(exprs: Seq[Expr]) extends Op {
    override type Self = And

    override def toString: String = productPrefix + exprs.mkString("(", ",", ")")

    override def foldOp[C](
                            eq: Eq => C,
                            le: Le => C,
                            leq: LEq => C,
                            not: Not => C,
                            and: And => C,
                            or: Or => C,
                          ): C = and(this)

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

    override def foldOp[C](
                            eq: Eq => C,
                            le: Le => C,
                            leq: LEq => C,
                            not: Not => C,
                            and: And => C,
                            or: Or => C,
                          ): C = or(this)

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
