package net.dafttech.f2s.ast

import cats.syntax.option._
import monix.eval.Coeval
import net.dafttech.f2s.ast.ColumnFilter.Logic

trait ColumnFilter {
  def &&(filter: ColumnFilter): Logic.And = Logic.And(this, filter)

  def ||(filter: ColumnFilter): Logic.Or = Logic.Or(this, filter)

  def xor(filter: ColumnFilter): Logic.Xor = Logic.Xor(this, filter)

  def unary_! : ColumnFilter = Logic.Not(this)


  def transform(f: ColumnFilter => Option[ColumnFilter]): ColumnFilter

  def string: String
}

object ColumnFilter {
  val empty: ColumnFilter = Logic.True

  sealed trait Logic extends ColumnFilter

  object Logic {

    case class And(a: ColumnFilter,
                   b: ColumnFilter) extends Logic {
      override def unary_! : ColumnFilter = Or(!a, !b)

      override def string: String = s"(${a.string} and ${b.string})"

      override def transform(f: ColumnFilter => Option[ColumnFilter]): ColumnFilter =
        f(this).getOrElse(And(a.transform(f), b.transform(f)))
    }

    object AndSeq {
      def apply(filters: Seq[ColumnFilter]): ColumnFilter =
        filters.reduceOption(And).getOrElse(Logic.True)

      def unapply(and: And): Option[Seq[ColumnFilter]] = unfold(and).value.some

      def unfold(and: And): Coeval[Seq[ColumnFilter]] = and match {
        case And(a: And, b: And) => Coeval.defer(for {
          aSeq <- unfold(a)
          bSeq <- unfold(b)
        } yield aSeq ++ bSeq)
        case And(a: And, b) => Coeval.defer(for (aSeq <- unfold(a)) yield aSeq :+ b)
        case And(a, b: And) => Coeval.defer(for (bSeq <- unfold(b)) yield a +: bSeq)
        case And(a, b) => Coeval.now(Seq(a, b))
      }
    }

    case class Or(a: ColumnFilter,
                  b: ColumnFilter) extends Logic {
      override def unary_! : ColumnFilter = And(!a, !b)

      override def string: String = s"(${a.string} or ${b.string})"

      override def transform(f: ColumnFilter => Option[ColumnFilter]): ColumnFilter =
        f(this).getOrElse(Or(a.transform(f), b.transform(f)))
    }

    object OrSeq {
      def apply(filters: Seq[ColumnFilter]): ColumnFilter =
        filters.reduceOption(Or).getOrElse(Logic.False)

      def unapply(or: Or): Option[Seq[ColumnFilter]] = unfold(or).value.some

      def unfold(or: Or): Coeval[Seq[ColumnFilter]] = or match {
        case Or(a: Or, b: Or) => Coeval.defer(for {
          aSeq <- unfold(a)
          bSeq <- unfold(b)
        } yield aSeq ++ bSeq)
        case Or(a: Or, b) => Coeval.defer(for (aSeq <- unfold(a)) yield aSeq :+ b)
        case Or(a, b: Or) => Coeval.defer(for (bSeq <- unfold(b)) yield a +: bSeq)
        case Or(a, b) => Coeval.now(Seq(a, b))
      }
    }

    case class Xor(a: ColumnFilter,
                   b: ColumnFilter) extends Logic {
      override def unary_! : ColumnFilter = Xor(!a, b)

      override def string: String = s"(${a.string} xor ${b.string})"

      override def transform(f: ColumnFilter => Option[ColumnFilter]): ColumnFilter =
        f(this).getOrElse(Xor(a.transform(f), b.transform(f)))
    }

    case class Not(filter: ColumnFilter) extends Logic {
      override def unary_! : ColumnFilter = filter

      override def string: String = s"not ${filter.string}"

      override def transform(f: ColumnFilter => Option[ColumnFilter]): ColumnFilter =
        f(this).getOrElse(Not(filter.transform(f)))
    }

    case object False extends Logic {
      override def unary_! : ColumnFilter = True

      override def string: String = "false"

      override def transform(f: ColumnFilter => Option[ColumnFilter]): ColumnFilter =
        f(this).getOrElse(False)
    }

    case object True extends Logic {
      override def unary_! : ColumnFilter = False

      override def string: String = "true"

      override def transform(f: ColumnFilter => Option[ColumnFilter]): ColumnFilter =
        f(this).getOrElse(True)
    }

  }


  sealed trait Expr

  trait Const extends Expr

  trait Ref extends Expr

  case class ConstValue(value: Value) extends Const

  case class ColName(name: String) extends Ref

  case class ColRef(column: Column[_]) extends Ref

  case class Comp(a: Expr, op: Op, b: Expr) extends ColumnFilter {
    override def unary_! : ColumnFilter = Comp(a, !op, b)

    override def string: String = s"""$a ${op.string} "$b""""

    override def transform(f: ColumnFilter => Option[ColumnFilter]): ColumnFilter =
      f(this).getOrElse(this)
  }

  object Comp {
    def unapply(comp: Comp): Option[(Expr, Op, Expr)] = Some((comp.a, comp.op, comp.b))
  }

  sealed trait Op {
    def string: String

    def unary_! : Op
  }

  object Op {

    case object Eq extends Op {
      override def string: String = "="

      override def unary_! : Op = NEq
    }

    case object NEq extends Op {
      override def string: String = "!="

      override def unary_! : Op = Eq
    }

    case object Le extends Op {
      override def string: String = "<"

      override def unary_! : Op = GEq
    }

    case object Gt extends Op {
      override def string: String = ">"

      override def unary_! : Op = LEq
    }

    case object LEq extends Op {
      override def string: String = "<="

      override def unary_! : Op = Gt
    }

    case object GEq extends Op {
      override def string: String = ">="

      override def unary_! : Op = Le
    }

  }
}