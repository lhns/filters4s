package net.dafttech.filters4s.ast

import cats.{Eval, Id}
import net.dafttech.filters4s.ast.Op._

object Simplify {
  def main(args: Array[String]): Unit = {
    val term = (Const.True === Const.False) && Const.True || Const.False
    println(term)
  }

  private def transformBottomUp[T](term: Expr): Eval[Expr] = term match { // TODO: Transform multiple levels?
    case op: Op =>
      op.foldToTerm[Eval](
        eq = {
          case Eq(ExprType.BoolType(a), ExprType.BoolType(b)) =>
            Eval.defer(Simplify(Not(XOr(a, b))))

          case eq =>
            Eval.now(eq)
        },
        le = {
          case le => Eval.now(le)
        },
        gt = {
          case Gt(a, b) => ???
        },
        leq = {
          case LEq(a, b) => ???
        },
        geq = {
          case GEq(a, b) => ???
        },
        not = {
          case Not(Not(term)) => Eval.defer(Simplify(term))
          case Not(And(terms)) => Eval.defer(Simplify(Or(terms.map(!_))))
          case Not(Or(terms)) => Eval.defer(Simplify(And(terms.map(!_))))
          case Not(term) => Simplify(term).map(Not(_))
        },
        and = {
          case And(terms) => ???
        },
        or = {
          case Or(terms) => ???
        },
      )

    case term => Eval.now(term)
  }

  private def transformTreeBottomUp(term: Expr): Eval[Expr] = {
    def transformOp(op: Op): Eval[Expr] =
      Eval.defer(op.transformOperands[Eval](transformTreeBottomUp(_).flatMap(transformBottomUp)))

    term match {
      case op: Op => transformOp(op)
      case term => Eval.now(term)
    }
  }

  def apply(term: Expr): Eval[Expr] = term match {
    case op: Op =>
      op.transformOperands[Id](identity)
      ??? // todo


    case term => Eval.now(term)
  }
}
