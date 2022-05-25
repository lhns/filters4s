package net.dafttech.filters4s.ast

import cats.Monad
import cats.instances.seq._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.traverse._
import net.dafttech.filters4s.ast.Op._

sealed abstract class Op extends Expr {
  type Self <: Op

  def fold[C](
               eq: Eq => C,
               le: Le => C,
               gt: Gt => C,
               leq: LEq => C,
               geq: GEq => C,
               not: Not => C,
               and: And => C,
               or: Or => C,
             ): C

  def foldToTerm[F[_] : Monad](
                                eq: Eq => F[Expr],
                                le: Le => F[Expr],
                                gt: Gt => F[Expr],
                                leq: LEq => F[Expr],
                                geq: GEq => F[Expr],
                                not: Not => F[Expr],
                                and: And => F[Expr],
                                or: Or => F[Expr],
                              ): F[Expr]

  def transformOperands[F[_] : Monad](f: Expr => F[Expr]): F[Self]
}

object Op {
  implicit class TermOps[T](val self: Expr) extends AnyVal {
    def ===(expr: Expr): Eq = Eq(self, expr)

    def =!=(expr: Expr): Not = Not(Eq(self, expr))

    def <(expr: Expr): Le = Le(self, expr)

    def >(expr: Expr): Gt = Gt(self, expr)

    def <=(expr: Expr): LEq = LEq(self, expr)

    def >=(expr: Expr): GEq = GEq(self, expr)

    def in(exprs: Seq[Expr]): In = In(self, exprs)

    def unary_! : Not = Not(self)

    def &&(expr: Expr): And = And(self, expr)

    def ||(expr: Expr): Or = Or(self, expr)

    def ^^(expr: Expr): XOr = XOr(self, expr)
  }

  case class Eq(a: Expr, b: Expr) extends Op {
    override type Self = Eq

    override def fold[C](
                          eq: Eq => C,
                          le: Le => C,
                          gt: Gt => C,
                          leq: LEq => C,
                          geq: GEq => C,
                          not: Not => C,
                          and: And => C,
                          or: Or => C,
                        ): C = eq(this)

    override def foldToTerm[F[_] : Monad](
                                           eq: Eq => F[Expr],
                                           le: Le => F[Expr],
                                           gt: Gt => F[Expr],
                                           leq: LEq => F[Expr],
                                           geq: GEq => F[Expr],
                                           not: Not => F[Expr],
                                           and: And => F[Expr],
                                           or: Or => F[Expr]
                                         ): F[Expr] =
      fold(eq, le, gt, leq, geq, not, and, or)

    override def transformOperands[F[_] : Monad](f: Expr => F[Expr]): F[Self] =
      for {
        a2 <- f(a)
        b2 <- f(b)
      } yield
        Eq(a2, b2)
  }

  case class Le(a: Expr, b: Expr) extends Op {
    override type Self = Le

    override def fold[C](
                          eq: Eq => C,
                          le: Le => C,
                          gt: Gt => C,
                          leq: LEq => C,
                          geq: GEq => C,
                          not: Not => C,
                          and: And => C,
                          or: Or => C,
                        ): C = le(this)

    override def foldToTerm[F[_] : Monad](
                                           eq: Eq => F[Expr],
                                           le: Le => F[Expr],
                                           gt: Gt => F[Expr],
                                           leq: LEq => F[Expr],
                                           geq: GEq => F[Expr],
                                           not: Not => F[Expr],
                                           and: And => F[Expr],
                                           or: Or => F[Expr]
                                         ): F[Expr] =
      fold(eq, le, gt, leq, geq, not, and, or)

    override def transformOperands[F[_] : Monad](f: Expr => F[Expr]): F[Self] =
      for {
        a2 <- f(a)
        b2 <- f(b)
      } yield
        Le(a2, b2)
  }

  case class Gt(a: Expr, b: Expr) extends Op {
    override type Self = Gt

    override def fold[C](
                          eq: Eq => C,
                          le: Le => C,
                          gt: Gt => C,
                          leq: LEq => C,
                          geq: GEq => C,
                          not: Not => C,
                          and: And => C,
                          or: Or => C,
                        ): C = gt(this)

    override def foldToTerm[F[_] : Monad](
                                           eq: Eq => F[Expr],
                                           le: Le => F[Expr],
                                           gt: Gt => F[Expr],
                                           leq: LEq => F[Expr],
                                           geq: GEq => F[Expr],
                                           not: Not => F[Expr],
                                           and: And => F[Expr],
                                           or: Or => F[Expr]
                                         ): F[Expr] =
      fold(eq, le, gt, leq, geq, not, and, or)

    override def transformOperands[F[_] : Monad](f: Expr => F[Expr]): F[Self] =
      for {
        a2 <- f(a)
        b2 <- f(b)
      } yield
        Gt(a2, b2)
  }

  case class LEq(a: Expr, b: Expr) extends Op {
    override type Self = LEq

    override def fold[C](
                          eq: Eq => C,
                          le: Le => C,
                          gt: Gt => C,
                          leq: LEq => C,
                          geq: GEq => C,
                          not: Not => C,
                          and: And => C,
                          or: Or => C,
                        ): C = leq(this)

    override def foldToTerm[F[_] : Monad](
                                           eq: Eq => F[Expr],
                                           le: Le => F[Expr],
                                           gt: Gt => F[Expr],
                                           leq: LEq => F[Expr],
                                           geq: GEq => F[Expr],
                                           not: Not => F[Expr],
                                           and: And => F[Expr],
                                           or: Or => F[Expr]
                                         ): F[Expr] =
      fold(eq, le, gt, leq, geq, not, and, or)

    override def transformOperands[F[_] : Monad](f: Expr => F[Expr]): F[Self] =
      for {
        a2 <- f(a)
        b2 <- f(b)
      } yield
        LEq(a2, b2)
  }

  case class GEq(a: Expr, b: Expr) extends Op {
    override type Self = GEq

    override def fold[C](
                          eq: Eq => C,
                          le: Le => C,
                          gt: Gt => C,
                          leq: LEq => C,
                          geq: GEq => C,
                          not: Not => C,
                          and: And => C,
                          or: Or => C,
                        ): C = geq(this)

    override def foldToTerm[F[_] : Monad](
                                           eq: Eq => F[Expr],
                                           le: Le => F[Expr],
                                           gt: Gt => F[Expr],
                                           leq: LEq => F[Expr],
                                           geq: GEq => F[Expr],
                                           not: Not => F[Expr],
                                           and: And => F[Expr],
                                           or: Or => F[Expr]
                                         ): F[Expr] =
      fold(eq, le, gt, leq, geq, not, and, or)

    override def transformOperands[F[_] : Monad](f: Expr => F[Expr]): F[Self] =
      for {
        a2 <- f(a)
        b2 <- f(b)
      } yield
        GEq(a2, b2)
  }

  case class In(a: Expr, bSeq: Seq[Expr]) extends Op {
    override type Self = In

    override def fold[C](
                          eq: Eq => C,
                          le: Le => C,
                          gt: Gt => C,
                          leq: LEq => C,
                          geq: GEq => C,
                          not: Not => C,
                          and: And => C,
                          or: Or => C,
                        ): C = or(Or(bSeq.map(b => a === b)))

    override def foldToTerm[F[_] : Monad](
                                           eq: Eq => F[Expr],
                                           le: Le => F[Expr],
                                           gt: Gt => F[Expr],
                                           leq: LEq => F[Expr],
                                           geq: GEq => F[Expr],
                                           not: Not => F[Expr],
                                           and: And => F[Expr],
                                           or: Or => F[Expr]
                                         ): F[Expr] =
      fold(eq, le, gt, leq, geq, not, and, or)

    override def transformOperands[F[_] : Monad](f: Expr => F[Expr]): F[Self] =
      for {
        a2 <- f(a)
        bSeq2 <- bSeq.map(f).sequence
      } yield
        In(a2, bSeq2)
  }

  case class Not(expr: Expr) extends Op {
    override type Self = Not

    override def fold[C](
                          eq: Eq => C,
                          le: Le => C,
                          gt: Gt => C,
                          leq: LEq => C,
                          geq: GEq => C,
                          not: Not => C,
                          and: And => C,
                          or: Or => C,
                        ): C = not(this)

    override def foldToTerm[F[_] : Monad](
                                           eq: Eq => F[Expr],
                                           le: Le => F[Expr],
                                           gt: Gt => F[Expr],
                                           leq: LEq => F[Expr],
                                           geq: GEq => F[Expr],
                                           not: Not => F[Expr],
                                           and: And => F[Expr],
                                           or: Or => F[Expr]
                                         ): F[Expr] =
      fold(eq, le, gt, leq, geq, not, and, or)

    override def transformOperands[F[_] : Monad](f: Expr => F[Expr]): F[Self] =
      for {
        expr2 <- f(expr)
      } yield
        Not(expr2)
  }

  case class And(exprs: Seq[Expr]) extends Op {
    override type Self = And

    override def toString: String = productPrefix + exprs.mkString("(", ",", ")")

    override def fold[C](
                          eq: Eq => C,
                          le: Le => C,
                          gt: Gt => C,
                          leq: LEq => C,
                          geq: GEq => C,
                          not: Not => C,
                          and: And => C,
                          or: Or => C,
                        ): C = and(this)

    override def foldToTerm[F[_] : Monad](
                                           eq: Eq => F[Expr],
                                           le: Le => F[Expr],
                                           gt: Gt => F[Expr],
                                           leq: LEq => F[Expr],
                                           geq: GEq => F[Expr],
                                           not: Not => F[Expr],
                                           and: And => F[Expr],
                                           or: Or => F[Expr]
                                         ): F[Expr] =
      fold(eq, le, gt, leq, geq, not, and, or)

    override def transformOperands[F[_] : Monad](f: Expr => F[Expr]): F[Self] =
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

    override def fold[C](
                          eq: Eq => C,
                          le: Le => C,
                          gt: Gt => C,
                          leq: LEq => C,
                          geq: GEq => C,
                          not: Not => C,
                          and: And => C,
                          or: Or => C,
                        ): C = or(this)

    override def foldToTerm[F[_] : Monad](
                                           eq: Eq => F[Expr],
                                           le: Le => F[Expr],
                                           gt: Gt => F[Expr],
                                           leq: LEq => F[Expr],
                                           geq: GEq => F[Expr],
                                           not: Not => F[Expr],
                                           and: And => F[Expr],
                                           or: Or => F[Expr]
                                         ): F[Expr] =
      fold(eq, le, gt, leq, geq, not, and, or)

    override def transformOperands[F[_] : Monad](f: Expr => F[Expr]): F[Self] =
      for {
        exprs2 <- exprs.map(f).sequence
      } yield
        Or(exprs2)
  }

  object Or {
    def apply(expr0: Expr, exprs: Expr*): Or = Or(expr0 +: exprs)
  }

  case class XOr(a: Expr, b: Expr) extends Op {
    override type Self = XOr

    override def fold[C](
                          eq: Eq => C,
                          le: Le => C,
                          gt: Gt => C,
                          leq: LEq => C,
                          geq: GEq => C,
                          not: Not => C,
                          and: And => C,
                          or: Or => C,
                        ): C = or((a && !b) || (!a && b))

    override def foldToTerm[F[_] : Monad](
                                           eq: Eq => F[Expr],
                                           le: Le => F[Expr],
                                           gt: Gt => F[Expr],
                                           leq: LEq => F[Expr],
                                           geq: GEq => F[Expr],
                                           not: Not => F[Expr],
                                           and: And => F[Expr],
                                           or: Or => F[Expr]
                                         ): F[Expr] =
      fold(eq, le, gt, leq, geq, not, and, or)

    override def transformOperands[F[_] : Monad](f: Expr => F[Expr]): F[Self] =
      for {
        a2 <- f(a)
        b2 <- f(b)
      } yield
        XOr(a2, b2)
  }

  case class XNOr(a: Expr, b: Expr) extends Op {
    override type Self = XNOr

    override def fold[C](
                          eq: Eq => C,
                          le: Le => C,
                          gt: Gt => C,
                          leq: LEq => C,
                          geq: GEq => C,
                          not: Not => C,
                          and: And => C,
                          or: Or => C,
                        ): C = or((a && b) || (!a && !b)) // TODO: TRANSFORM MULTIPLE LEVELS

    override def foldToTerm[F[_] : Monad](
                                           eq: Eq => F[Expr],
                                           le: Le => F[Expr],
                                           gt: Gt => F[Expr],
                                           leq: LEq => F[Expr],
                                           geq: GEq => F[Expr],
                                           not: Not => F[Expr],
                                           and: And => F[Expr],
                                           or: Or => F[Expr]
                                         ): F[Expr] =
      fold(eq, le, gt, leq, geq, not, and, or)

    override def transformOperands[F[_] : Monad](f: Expr => F[Expr]): F[Self] =
      for {
        a2 <- f(a)
        b2 <- f(b)
      } yield
        XNOr(a2, b2)
  }
}
