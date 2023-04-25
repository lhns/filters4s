package net.dafttech.filters4s.ast

import cats.{Eval, Id}
import net.dafttech.filters4s.ast.Op._

object Simplify {
  def main(args: Array[String]): Unit = {
    val expr = (Const.True === Const.False) && Const.True || Const.False
    println(expr)
  }

  private def transformBottomUp(expr: Expr): Eval[Expr] = expr match { // TODO: Transform multiple levels?
    case op: Op =>
      op.foldToExpr[Eval](
        eq = {
          case Eq(ExprType.BoolType(a), ExprType.BoolType(b)) =>
            Eval.defer(Simplify((a && b) || (!a && !b)))

          case Eq(Ref(a), Ref(b)) if a == b =>
            Eval.now(Const.True)

          case eq =>
            Eval.now(eq)
        },
        le = {
          case Le(ExprType.BoolType(a), ExprType.BoolType(b)) =>
            Eval.defer(Simplify(!a && b))

          case le =>
            Eval.now(le)
        },
        //gt = {},
        leq = {
          case LEq(ExprType.BoolType(a), ExprType.BoolType(b)) =>
            Eval.defer(Simplify(!a || b))

          case lEq =>
            Eval.now(lEq)
        },
        //geq = {},
        not = {
          case Not(Not(expr)) => Eval.defer(Simplify(expr))
          case Not(And(exprs)) => Eval.defer(Simplify(Or(exprs.map(!_))))
          case Not(Or(exprs)) => Eval.defer(Simplify(And(exprs.map(!_))))
          //case Not(Le(a, b)) => Eval.defer(Simplify(GEq(a, b)))
          //case Not(Gt(a, b)) => Eval.defer(Simplify(LEq(a, b)))
          //case Not(LEq(a, b)) => Eval.defer(Simplify(Gt(a, b)))
          //case Not(GEq(a, b)) => Eval.defer(Simplify(Le(a, b)))
          case Not(Const.True) => Eval.now(Const.False)
          case Not(Const.False) => Eval.now(Const.True)
          case Not(expr) => Simplify(expr).map(Not(_))
        },
        and = {
          case And(exprs) if exprs.contains(Const.False) =>
            Eval.now(Const.False)
          case And(exprs) =>
            Eval.now(And(exprs.flatMap {
              case And(exprs) => exprs
              case expr => List(expr)
            }))
        },
        or = {
          case Or(exprs) if exprs.contains(Const.True) =>
            Eval.now(Const.True)
          case Or(exprs) =>
            Eval.now(Or(exprs.flatMap {
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
