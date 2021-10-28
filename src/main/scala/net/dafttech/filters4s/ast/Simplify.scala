package net.dafttech.filters4s.ast

import cats.{Eval, Id}
import net.dafttech.filters4s.ast.Op._

object Simplify {
  def main(args: Array[String]): Unit = {
    val term = (Const.True === Const.False) && Const.True || Const.False
    println(term)
  }

  private def transformBottomUp[T](term: Term[T]): Eval[Term[T]] = term match { // TODO: Transform multiple levels?
    case op: Op[_, T] =>
      op.foldToTerm[Eval](
        eq = {
          case eq if eq.operandTermType == TermType.BoolType =>
            val Eq(a, b) = eq.asInstanceOf[Eq[Boolean]]
            Eval.defer(Simplify(XNOr(a, b)))

          case eq =>
            Eval.now(eq)
        },
        neq = {
          case neq if neq.operandTermType == TermType.BoolType =>
            val NEq(a, b) = neq.asInstanceOf[NEq[Boolean]]
            Eval.defer(Simplify(XOr(a, b)))

          case neq =>
            Eval.now(neq)
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

  private def transformTreeBottomUp[T](term: Term[T]): Eval[Term[T]] = {
    def transformOp[A](op: Op[A, T]): Eval[Term[T]] =
      Eval.defer(op.transformOperands[Eval](transformTreeBottomUp(_).flatMap(transformBottomUp)))

    term match {
      case op: Op[_, T] => transformOp(op)
      case term => Eval.now(term)
    }
  }

  def apply[T](term: Term[T]): Eval[Term[T]] = term match {
    case op: Op[_, T] =>
      op.transformOperands[Id](identity)


    case term => Eval.now(term)
  }
}
