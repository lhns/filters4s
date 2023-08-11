package net.dafttech.filters4s.ast

import cats.Eval
import cats.syntax.apply._
import cats.syntax.traverse._
import net.dafttech.filters4s.ast.Op._

object Simplify {
  private def simplifyComparisons(expr: Expr): Eval[Expr] = Eval.defer(expr match {
    case Eq(a, b) if a == b =>
      Eval.now(Const.True)

    case Le(a, b) if a == b =>
      Eval.now(Const.False)

    case LEq(a, b) if a == b =>
      Eval.now(Const.True)

    case Eq(ExprType.BoolType(a), ExprType.BoolType(b)) =>
      (simplifyComparisons(a), simplifyComparisons(b)).tupled.map {
        case (a, b) =>
          (a && b) || (!a && !b)
      }

    case Le(ExprType.BoolType(a), ExprType.BoolType(b)) =>
      (simplifyComparisons(a), simplifyComparisons(b)).tupled.map {
        case (a, b) =>
          !a && b
      }

    case LEq(ExprType.BoolType(a), ExprType.BoolType(b)) =>
      (simplifyComparisons(a), simplifyComparisons(b)).tupled.map {
        case (a, b) =>
          !a || b
      }

    case op: Op =>
      op.transformOperandsF(simplifyComparisons)

    case expr =>
      Eval.now(expr)
  })

  private def combNegations(expr: Expr): Eval[Expr] = Eval.defer(expr match {
    case Not(Not(expr)) =>
      combNegations(expr)

    case Not(And(exprs)) =>
      exprs.map(e => combNegations(!e)).sequence.map(Or(_))

    case Not(Or(exprs)) =>
      exprs.map(e => combNegations(!e)).sequence.map(And(_))

    case Not(Const.True) =>
      Eval.now(Const.False)

    case Not(Const.False) =>
      Eval.now(Const.True)

    case op: Op =>
      op.transformOperandsF(combNegations)

    case expr =>
      Eval.now(expr)
  })

  private def simplifyAndOr(expr: Expr): Eval[Expr] = Eval.defer(expr match {
    case op: Op =>
      op.transformOperandsF(simplifyAndOr).flatMap {
        case And(exprs) if exprs.contains(Const.False) =>
          Eval.now(Const.False)

        case And(exprs) if exprs.forall(_ == Const.True) =>
          Eval.now(Const.True)

        case And(exprs) =>
          Eval.now(And(exprs.flatMap {
            case And(exprs) => exprs
            case expr => List(expr)
          }))

        case Or(exprs) if exprs.contains(Const.True) =>
          Eval.now(Const.True)

        case Or(exprs) if exprs.forall(_ == Const.False) =>
          Eval.now(Const.False)

        case Or(exprs) =>
          Eval.now(Or(exprs.flatMap {
            case Or(exprs) => exprs
            case expr => List(expr)
          }))

        case op =>
          Eval.now(op)
      }

    case expr =>
      Eval.now(expr)
  })

  def apply(expr: Expr): Eval[Expr] =
    simplifyComparisons(expr)
      .flatMap(combNegations)
      .flatMap(simplifyAndOr)
}
