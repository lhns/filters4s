package de.lhns.filters4s.doobie

import cats.Eval
import cats.data.NonEmptyList
import cats.syntax.apply._
import cats.syntax.traverse._
import de.lhns.filters4s.ast.Op.{And, Eq, Gt, In, Like, Lt, Not, Or}
import de.lhns.filters4s.ast.{Const, Expr, ExprType, Ref}
import doobie._
import doobie.implicits._

object DoobieCompiler {
  def compile(expr: Expr): Eval[Fragment] = Eval.defer(expr match {
    case const@Const(value, tpe) =>
      //TODO
      Eval.now(const match {
        case ExprType.NullType(Const((), _)) => fr"NULL"
        case ExprType.StringType(Const(string, _)) => fr"$string"
        case ExprType.NumberType(Const(number, _)) => fr"$number"
        case ExprType.BoolType(Const(boolean, _)) => if (boolean) fr"TRUE" else fr"FALSE"
        case _ => throw new RuntimeException(s"unsupported db type: $value")
      })

    case In(a, bSeq) =>
      (compile(a), bSeq.map(compile).sequence).tupled.map { case (a, bSeq) =>
        NonEmptyList.fromFoldable(bSeq).fold(fr"FALSE")(bSeq =>
          Fragments.parentheses(a ++ fr"IN" ++ Fragments.parentheses(Fragments.comma(bSeq)))
        )
      }

    case Ref(name, tpeOption) =>
      Eval.now(Fragment.const(s"\"$name\""))

    case Eq(a, b) =>
      (compile(a), compile(b)).tupled.map { case (a, b) =>
        Fragments.parentheses(fr0"$a = $b")
      }

    case Lt(a, b) =>
      (compile(a), compile(b)).tupled.map { case (a, b) =>
        Fragments.parentheses(fr0"$a < $b")
      }

    case Gt(a, b) =>
      (compile(a), compile(b)).tupled.map { case (a, b) =>
        Fragments.parentheses(fr0"$a > $b")
      }

    case Not(Eq(a, b)) =>
      (compile(a), compile(b)).tupled.map { case (a, b) =>
        Fragments.parentheses(fr0"$a != $b")
      }

    case Not(Lt(a, b)) =>
      (compile(a), compile(b)).tupled.map { case (a, b) =>
        Fragments.parentheses(fr0"$a >= $b")
      }

    case Not(Gt(a, b)) =>
      (compile(a), compile(b)).tupled.map { case (a, b) =>
        Fragments.parentheses(fr0"$a <= $b")
      }

    case Like(a, b) =>
      (compile(a), compile(b)).tupled.map { case (a, b) =>
        Fragments.parentheses(fr0"$a like $b")
      }

    case Not(expr) =>
      compile(expr).map(e => fr0"!$e")

    case And(exprs) =>
      exprs.map(compile).sequence.map(Fragments.andFallbackTrue(_))

    case Or(exprs) =>
      exprs.map(compile).sequence.map(Fragments.orFallbackFalse(_))
  })
}
