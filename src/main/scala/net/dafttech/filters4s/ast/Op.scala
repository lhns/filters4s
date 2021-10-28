package net.dafttech.filters4s.ast

import cats.Monad
import cats.instances.seq._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.traverse._
import net.dafttech.filters4s.ast.Op._

sealed abstract class Op[A, B: TermType](implicit val operandTermType: TermType[A]) extends Term[B] {
  type Self <: Op[A, B]

  def fold[C](
               eq: Eq[A] => C,
               neq: NEq[A] => C,
               le: Le[A] => C,
               gt: Gt[A] => C,
               leq: LEq[A] => C,
               geq: GEq[A] => C,
               not: Not => C,
               and: And => C,
               or: Or => C,
             ): C

  def foldToTerm[F[_] : Monad](
                                eq: Eq[A] => F[Term[Boolean]],
                                neq: NEq[A] => F[Term[Boolean]],
                                le: Le[A] => F[Term[Boolean]],
                                gt: Gt[A] => F[Term[Boolean]],
                                leq: LEq[A] => F[Term[Boolean]],
                                geq: GEq[A] => F[Term[Boolean]],
                                not: Not => F[Term[Boolean]],
                                and: And => F[Term[Boolean]],
                                or: Or => F[Term[Boolean]],
                              ): F[Term[B]]

  def transformOperands[F[_] : Monad](f: Term[A] => F[Term[A]]): F[Self]
}

object Op {
  implicit class TermOps[T](val self: Term[T]) extends AnyVal {
    def ===(term: Term[T]): Eq[T] = Eq(self, term)(self.termType)

    def =!=(term: Term[T]): NEq[T] = NEq(self, term)(self.termType)

    def <(term: Term[T]): Le[T] = Le(self, term)(self.termType)

    def >(term: Term[T]): Gt[T] = Gt(self, term)(self.termType)

    def <=(term: Term[T]): LEq[T] = LEq(self, term)(self.termType)

    def >=(term: Term[T]): GEq[T] = GEq(self, term)(self.termType)

    def in(terms: Seq[Term[T]]): In[T] = In(self, terms)(self.termType)
  }

  implicit class BooleanTermOps(val self: Term[Boolean]) extends AnyVal {
    def unary_! : Not = Not(self)

    def &&(term: Term[Boolean]): And = And(self, term)

    def ||(term: Term[Boolean]): Or = Or(self, term)

    def ^^(term: Term[Boolean]): XOr = XOr(self, term)
  }

  case class Eq[A: TermType](a: Term[A], b: Term[A]) extends Op[A, Boolean] {
    override type Self = Eq[A]

    override def fold[C](
                          eq: Eq[A] => C,
                          neq: NEq[A] => C,
                          le: Le[A] => C,
                          gt: Gt[A] => C,
                          leq: LEq[A] => C,
                          geq: GEq[A] => C,
                          not: Not => C,
                          and: And => C,
                          or: Or => C,
                        ): C = eq(this)

    override def foldToTerm[F[_] : Monad](
                                           eq: Eq[A] => F[Term[Boolean]],
                                           neq: NEq[A] => F[Term[Boolean]],
                                           le: Le[A] => F[Term[Boolean]],
                                           gt: Gt[A] => F[Term[Boolean]],
                                           leq: LEq[A] => F[Term[Boolean]],
                                           geq: GEq[A] => F[Term[Boolean]],
                                           not: Not => F[Term[Boolean]],
                                           and: And => F[Term[Boolean]],
                                           or: Or => F[Term[Boolean]]
                                         ): F[Term[Boolean]] =
      fold(eq, neq, le, gt, leq, geq, not, and, or)

    override def transformOperands[F[_] : Monad](f: Term[A] => F[Term[A]]): F[Self] =
      for {
        a2 <- f(a)
        b2 <- f(b)
      } yield
        Eq(a2, b2)
  }

  case class NEq[A: TermType](a: Term[A], b: Term[A]) extends Op[A, Boolean] {
    override type Self = NEq[A]

    override def fold[C](
                          eq: Eq[A] => C,
                          neq: NEq[A] => C,
                          le: Le[A] => C,
                          gt: Gt[A] => C,
                          leq: LEq[A] => C,
                          geq: GEq[A] => C,
                          not: Not => C,
                          and: And => C,
                          or: Or => C,
                        ): C = neq(this)

    override def foldToTerm[F[_] : Monad](
                                           eq: Eq[A] => F[Term[Boolean]],
                                           neq: NEq[A] => F[Term[Boolean]],
                                           le: Le[A] => F[Term[Boolean]],
                                           gt: Gt[A] => F[Term[Boolean]],
                                           leq: LEq[A] => F[Term[Boolean]],
                                           geq: GEq[A] => F[Term[Boolean]],
                                           not: Not => F[Term[Boolean]],
                                           and: And => F[Term[Boolean]],
                                           or: Or => F[Term[Boolean]]
                                         ): F[Term[Boolean]] =
      fold(eq, neq, le, gt, leq, geq, not, and, or)

    override def transformOperands[F[_] : Monad](f: Term[A] => F[Term[A]]): F[Self] =
      for {
        a2 <- f(a)
        b2 <- f(b)
      } yield
        NEq(a2, b2)
  }

  case class Le[A: TermType](a: Term[A], b: Term[A]) extends Op[A, Boolean] {
    override type Self = Le[A]

    override def fold[C](
                          eq: Eq[A] => C,
                          neq: NEq[A] => C,
                          le: Le[A] => C,
                          gt: Gt[A] => C,
                          leq: LEq[A] => C,
                          geq: GEq[A] => C,
                          not: Not => C,
                          and: And => C,
                          or: Or => C,
                        ): C = le(this)

    override def foldToTerm[F[_] : Monad](
                                           eq: Eq[A] => F[Term[Boolean]],
                                           neq: NEq[A] => F[Term[Boolean]],
                                           le: Le[A] => F[Term[Boolean]],
                                           gt: Gt[A] => F[Term[Boolean]],
                                           leq: LEq[A] => F[Term[Boolean]],
                                           geq: GEq[A] => F[Term[Boolean]],
                                           not: Not => F[Term[Boolean]],
                                           and: And => F[Term[Boolean]],
                                           or: Or => F[Term[Boolean]]
                                         ): F[Term[Boolean]] =
      fold(eq, neq, le, gt, leq, geq, not, and, or)

    override def transformOperands[F[_] : Monad](f: Term[A] => F[Term[A]]): F[Self] =
      for {
        a2 <- f(a)
        b2 <- f(b)
      } yield
        Le(a2, b2)
  }

  case class Gt[A: TermType](a: Term[A], b: Term[A]) extends Op[A, Boolean] {
    override type Self = Gt[A]

    override def fold[C](
                          eq: Eq[A] => C,
                          neq: NEq[A] => C,
                          le: Le[A] => C,
                          gt: Gt[A] => C,
                          leq: LEq[A] => C,
                          geq: GEq[A] => C,
                          not: Not => C,
                          and: And => C,
                          or: Or => C,
                        ): C = gt(this)

    override def foldToTerm[F[_] : Monad](
                                           eq: Eq[A] => F[Term[Boolean]],
                                           neq: NEq[A] => F[Term[Boolean]],
                                           le: Le[A] => F[Term[Boolean]],
                                           gt: Gt[A] => F[Term[Boolean]],
                                           leq: LEq[A] => F[Term[Boolean]],
                                           geq: GEq[A] => F[Term[Boolean]],
                                           not: Not => F[Term[Boolean]],
                                           and: And => F[Term[Boolean]],
                                           or: Or => F[Term[Boolean]]
                                         ): F[Term[Boolean]] =
      fold(eq, neq, le, gt, leq, geq, not, and, or)

    override def transformOperands[F[_] : Monad](f: Term[A] => F[Term[A]]): F[Self] =
      for {
        a2 <- f(a)
        b2 <- f(b)
      } yield
        Gt(a2, b2)
  }

  case class LEq[A: TermType](a: Term[A], b: Term[A]) extends Op[A, Boolean] {
    override type Self = LEq[A]

    override def fold[C](
                          eq: Eq[A] => C,
                          neq: NEq[A] => C,
                          le: Le[A] => C,
                          gt: Gt[A] => C,
                          leq: LEq[A] => C,
                          geq: GEq[A] => C,
                          not: Not => C,
                          and: And => C,
                          or: Or => C,
                        ): C = leq(this)

    override def foldToTerm[F[_] : Monad](
                                           eq: Eq[A] => F[Term[Boolean]],
                                           neq: NEq[A] => F[Term[Boolean]],
                                           le: Le[A] => F[Term[Boolean]],
                                           gt: Gt[A] => F[Term[Boolean]],
                                           leq: LEq[A] => F[Term[Boolean]],
                                           geq: GEq[A] => F[Term[Boolean]],
                                           not: Not => F[Term[Boolean]],
                                           and: And => F[Term[Boolean]],
                                           or: Or => F[Term[Boolean]]
                                         ): F[Term[Boolean]] =
      fold(eq, neq, le, gt, leq, geq, not, and, or)

    override def transformOperands[F[_] : Monad](f: Term[A] => F[Term[A]]): F[Self] =
      for {
        a2 <- f(a)
        b2 <- f(b)
      } yield
        LEq(a2, b2)
  }

  case class GEq[A: TermType](a: Term[A], b: Term[A]) extends Op[A, Boolean] {
    override type Self = GEq[A]

    override def fold[C](
                          eq: Eq[A] => C,
                          neq: NEq[A] => C,
                          le: Le[A] => C,
                          gt: Gt[A] => C,
                          leq: LEq[A] => C,
                          geq: GEq[A] => C,
                          not: Not => C,
                          and: And => C,
                          or: Or => C,
                        ): C = geq(this)

    override def foldToTerm[F[_] : Monad](
                                           eq: Eq[A] => F[Term[Boolean]],
                                           neq: NEq[A] => F[Term[Boolean]],
                                           le: Le[A] => F[Term[Boolean]],
                                           gt: Gt[A] => F[Term[Boolean]],
                                           leq: LEq[A] => F[Term[Boolean]],
                                           geq: GEq[A] => F[Term[Boolean]],
                                           not: Not => F[Term[Boolean]],
                                           and: And => F[Term[Boolean]],
                                           or: Or => F[Term[Boolean]]
                                         ): F[Term[Boolean]] =
      fold(eq, neq, le, gt, leq, geq, not, and, or)

    override def transformOperands[F[_] : Monad](f: Term[A] => F[Term[A]]): F[Self] =
      for {
        a2 <- f(a)
        b2 <- f(b)
      } yield
        GEq(a2, b2)
  }

  case class In[A: TermType](a: Term[A], bSeq: Seq[Term[A]]) extends Op[A, Boolean] {
    override type Self = In[A]

    override def fold[C](
                          eq: Eq[A] => C,
                          neq: NEq[A] => C,
                          le: Le[A] => C,
                          gt: Gt[A] => C,
                          leq: LEq[A] => C,
                          geq: GEq[A] => C,
                          not: Not => C,
                          and: And => C,
                          or: Or => C,
                        ): C = or(Or(bSeq.map(b => a === b)))

    override def foldToTerm[F[_] : Monad](
                                           eq: Eq[A] => F[Term[Boolean]],
                                           neq: NEq[A] => F[Term[Boolean]],
                                           le: Le[A] => F[Term[Boolean]],
                                           gt: Gt[A] => F[Term[Boolean]],
                                           leq: LEq[A] => F[Term[Boolean]],
                                           geq: GEq[A] => F[Term[Boolean]],
                                           not: Not => F[Term[Boolean]],
                                           and: And => F[Term[Boolean]],
                                           or: Or => F[Term[Boolean]]
                                         ): F[Term[Boolean]] =
      fold(eq, neq, le, gt, leq, geq, not, and, or)

    override def transformOperands[F[_] : Monad](f: Term[A] => F[Term[A]]): F[Self] =
      for {
        a2 <- f(a)
        bSeq2 <- bSeq.map(f).sequence
      } yield
        In(a2, bSeq2)
  }

  case class Not(term: Term[Boolean]) extends Op[Boolean, Boolean] {
    override type Self = Not

    override def fold[C](
                          eq: Eq[Boolean] => C,
                          neq: NEq[Boolean] => C,
                          le: Le[Boolean] => C,
                          gt: Gt[Boolean] => C,
                          leq: LEq[Boolean] => C,
                          geq: GEq[Boolean] => C,
                          not: Not => C,
                          and: And => C,
                          or: Or => C,
                        ): C = not(this)

    override def foldToTerm[F[_] : Monad](
                                           eq: Eq[Boolean] => F[Term[Boolean]],
                                           neq: NEq[Boolean] => F[Term[Boolean]],
                                           le: Le[Boolean] => F[Term[Boolean]],
                                           gt: Gt[Boolean] => F[Term[Boolean]],
                                           leq: LEq[Boolean] => F[Term[Boolean]],
                                           geq: GEq[Boolean] => F[Term[Boolean]],
                                           not: Not => F[Term[Boolean]],
                                           and: And => F[Term[Boolean]],
                                           or: Or => F[Term[Boolean]]
                                         ): F[Term[Boolean]] =
      fold(eq, neq, le, gt, leq, geq, not, and, or)

    override def transformOperands[F[_] : Monad](f: Term[Boolean] => F[Term[Boolean]]): F[Self] =
      for {
        term2 <- f(term)
      } yield
        Not(term2)
  }

  case class And(terms: Seq[Term[Boolean]]) extends Op[Boolean, Boolean] {
    override type Self = And

    override def toString: String = productPrefix + terms.mkString("(", ",", ")")

    override def fold[C](
                          eq: Eq[Boolean] => C,
                          neq: NEq[Boolean] => C,
                          le: Le[Boolean] => C,
                          gt: Gt[Boolean] => C,
                          leq: LEq[Boolean] => C,
                          geq: GEq[Boolean] => C,
                          not: Not => C,
                          and: And => C,
                          or: Or => C,
                        ): C = and(this)

    override def foldToTerm[F[_] : Monad](
                                           eq: Eq[Boolean] => F[Term[Boolean]],
                                           neq: NEq[Boolean] => F[Term[Boolean]],
                                           le: Le[Boolean] => F[Term[Boolean]],
                                           gt: Gt[Boolean] => F[Term[Boolean]],
                                           leq: LEq[Boolean] => F[Term[Boolean]],
                                           geq: GEq[Boolean] => F[Term[Boolean]],
                                           not: Not => F[Term[Boolean]],
                                           and: And => F[Term[Boolean]],
                                           or: Or => F[Term[Boolean]]
                                         ): F[Term[Boolean]] =
      fold(eq, neq, le, gt, leq, geq, not, and, or)

    override def transformOperands[F[_] : Monad](f: Term[Boolean] => F[Term[Boolean]]): F[Self] =
      for {
        terms2 <- terms.map(f).sequence
      } yield
        And(terms2)
  }

  object And {
    def apply(term0: Term[Boolean], terms: Term[Boolean]*): And = And(term0 +: terms)
  }

  case class Or(terms: Seq[Term[Boolean]]) extends Op[Boolean, Boolean] {
    override type Self = Or

    override def toString: String = productPrefix + terms.mkString("(", ",", ")")

    override def fold[C](
                          eq: Eq[Boolean] => C,
                          neq: NEq[Boolean] => C,
                          le: Le[Boolean] => C,
                          gt: Gt[Boolean] => C,
                          leq: LEq[Boolean] => C,
                          geq: GEq[Boolean] => C,
                          not: Not => C,
                          and: And => C,
                          or: Or => C,
                        ): C = or(this)

    override def foldToTerm[F[_] : Monad](
                                           eq: Eq[Boolean] => F[Term[Boolean]],
                                           neq: NEq[Boolean] => F[Term[Boolean]],
                                           le: Le[Boolean] => F[Term[Boolean]],
                                           gt: Gt[Boolean] => F[Term[Boolean]],
                                           leq: LEq[Boolean] => F[Term[Boolean]],
                                           geq: GEq[Boolean] => F[Term[Boolean]],
                                           not: Not => F[Term[Boolean]],
                                           and: And => F[Term[Boolean]],
                                           or: Or => F[Term[Boolean]]
                                         ): F[Term[Boolean]] =
      fold(eq, neq, le, gt, leq, geq, not, and, or)

    override def transformOperands[F[_] : Monad](f: Term[Boolean] => F[Term[Boolean]]): F[Self] =
      for {
        terms2 <- terms.map(f).sequence
      } yield
        Or(terms2)
  }

  object Or {
    def apply(term0: Term[Boolean], terms: Term[Boolean]*): Or = Or(term0 +: terms)
  }

  case class XOr(a: Term[Boolean], b: Term[Boolean]) extends Op[Boolean, Boolean] {
    override type Self = XOr

    override def fold[C](
                          eq: Eq[Boolean] => C,
                          neq: NEq[Boolean] => C,
                          le: Le[Boolean] => C,
                          gt: Gt[Boolean] => C,
                          leq: LEq[Boolean] => C,
                          geq: GEq[Boolean] => C,
                          not: Not => C,
                          and: And => C,
                          or: Or => C,
                        ): C = or((a && !b) || (!a && b))

    override def foldToTerm[F[_] : Monad](
                                           eq: Eq[Boolean] => F[Term[Boolean]],
                                           neq: NEq[Boolean] => F[Term[Boolean]],
                                           le: Le[Boolean] => F[Term[Boolean]],
                                           gt: Gt[Boolean] => F[Term[Boolean]],
                                           leq: LEq[Boolean] => F[Term[Boolean]],
                                           geq: GEq[Boolean] => F[Term[Boolean]],
                                           not: Not => F[Term[Boolean]],
                                           and: And => F[Term[Boolean]],
                                           or: Or => F[Term[Boolean]]
                                         ): F[Term[Boolean]] =
      fold(eq, neq, le, gt, leq, geq, not, and, or)

    override def transformOperands[F[_] : Monad](f: Term[Boolean] => F[Term[Boolean]]): F[Self] =
      for {
        a2 <- f(a)
        b2 <- f(b)
      } yield
        XOr(a2, b2)
  }

  case class XNOr(a: Term[Boolean], b: Term[Boolean]) extends Op[Boolean, Boolean] {
    override type Self = XNOr

    override def fold[C](
                          eq: Eq[Boolean] => C,
                          neq: NEq[Boolean] => C,
                          le: Le[Boolean] => C,
                          gt: Gt[Boolean] => C,
                          leq: LEq[Boolean] => C,
                          geq: GEq[Boolean] => C,
                          not: Not => C,
                          and: And => C,
                          or: Or => C,
                        ): C = or((a && b) || (!a && !b)) // TODO: TRANSFORM MULTIPLE LEVELS

    override def foldToTerm[F[_] : Monad](
                                           eq: Eq[Boolean] => F[Term[Boolean]],
                                           neq: NEq[Boolean] => F[Term[Boolean]],
                                           le: Le[Boolean] => F[Term[Boolean]],
                                           gt: Gt[Boolean] => F[Term[Boolean]],
                                           leq: LEq[Boolean] => F[Term[Boolean]],
                                           geq: GEq[Boolean] => F[Term[Boolean]],
                                           not: Not => F[Term[Boolean]],
                                           and: And => F[Term[Boolean]],
                                           or: Or => F[Term[Boolean]]
                                         ): F[Term[Boolean]] =
      fold(eq, neq, le, gt, leq, geq, not, and, or)

    override def transformOperands[F[_] : Monad](f: Term[Boolean] => F[Term[Boolean]]): F[Self] =
      for {
        a2 <- f(a)
        b2 <- f(b)
      } yield
        XNOr(a2, b2)
  }
}
