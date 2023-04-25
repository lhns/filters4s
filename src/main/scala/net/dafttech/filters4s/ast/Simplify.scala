package net.dafttech.filters4s.ast

import cats.{Eval, Id}
import net.dafttech.filters4s.ast.Op._

object Simplify {
  def main(args: Array[String]): Unit = {
    val expr = (Const.True === Const.False) && Const.True || Const.False
    println(expr)
  }

  private def transformBottomUp[T](expr: Expr): Eval[Expr] = expr match { // TODO: Transform multiple levels?
    case op: Op =>
      op.foldToExpr[Eval](
        eq = {
          case Eq(ExprType.BoolType(a), ExprType.BoolType(b)) =>
            Eval.defer(Simplify((a && b) || (!a && !b)))

          case eq =>
            Eval.now(eq)
        },
        le = { le =>
          Eval.now(le)
        },
        gt = { case Gt(a, b) =>
          ???
        },
        leq = { case LEq(a, b) =>
          ???
        },
        geq = { case GEq(a, b) =>
          ???
        },
        not = {
          case Not(Not(expr)) => Eval.defer(Simplify(expr))
          case Not(And(exprs)) => Eval.defer(Simplify(Or(exprs.map(!_))))
          case Not(Or(exprs)) => Eval.defer(Simplify(And(exprs.map(!_))))
          case Not(Le(a, b)) => Eval.defer(Simplify(GEq(a, b)))
          case Not(Gt(a, b)) => Eval.defer(Simplify(LEq(a, b)))
          case Not(LEq(a, b)) => Eval.defer(Simplify(Gt(a, b)))
          case Not(GEq(a, b)) => Eval.defer(Simplify(Le(a, b)))
          case Not(expr) => Simplify(expr).map(Not(_))
        },
        and = { case And(exprs) =>
          Eval.defer(And(exprs.flatMap {
            case And(exprs) => exprs
            case expr => List(expr)
          }))
        },
        or = { case Or(exprs) =>
          Eval.defer(Or(exprs.flatMap {
            case Or(exprs) => exprs
            case expr => List(expr)
          }))
        },
      )

    case expr => Eval.now(expr)
  }

  private def transformTreeBottomUp(expr: Expr): Eval[Expr] = {
    def transformOp(op: Op): Eval[Expr] =
      Eval.defer(op.transformOperands[Eval](transformTreeBottomUp(_).flatMap(transformBottomUp)))

    expr match {
      case op: Op => transformOp(op)
      case expr => Eval.now(expr)
    }
  }

  def apply(expr: Expr): Eval[Expr] = expr match {
    case op: Op =>
      op.transformOperands[Id](identity)
      ??? // todo


    case expr => Eval.now(expr)
  }
}
