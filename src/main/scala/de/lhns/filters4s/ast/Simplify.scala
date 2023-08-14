package de.lhns.filters4s.ast

import cats.Eval
import cats.syntax.apply._
import cats.syntax.traverse._
import de.lhns.filters4s.ast.Op._

object Simplify {
  private def simplifyComparisons(expr: Expr): Eval[Expr] = Eval.defer(expr match {
    case Eq(a, b) if a == b =>
      Eval.now(Const.True)

    case Lt(a, b) if a == b =>
      Eval.now(Const.False)

    case Gt(a, b) if a == b =>
      Eval.now(Const.False)

    case Eq(a@Const(_, _), b@Const(_, _)) =>
      Eval.now(a === b)

    case Lt(a@Const(_, _), b@Const(_, _)) =>
      Eval.now(a < b)

    case Gt(a@Const(_, _), b@Const(_, _)) =>
      Eval.now(a > b)

    // TODO: case In(a, bSeq) =>

    case Eq(ExprType.BoolType(a), ExprType.BoolType(b)) =>
      (simplifyComparisons(a), simplifyComparisons(b)).tupled.map {
        case (a, b) =>
          (a && b) || (!a && !b)
      }

    case Lt(ExprType.BoolType(a), ExprType.BoolType(b)) =>
      (simplifyComparisons(a), simplifyComparisons(b)).tupled.map {
        case (a, b) =>
          !a && b
      }

    case Gt(ExprType.BoolType(a), ExprType.BoolType(b)) =>
      (simplifyComparisons(a), simplifyComparisons(b)).tupled.map {
        case (a, b) =>
          a && !b
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

        case And(exprs) =>
          val newExprs = exprs.flatMap {
            case And(exprs) => exprs
            case expr => List(expr)
          }.filterNot(_ == Const.True)

          Eval.now {
            if (newExprs.isEmpty) Const.True
            else if (newExprs.sizeIs == 1) newExprs.head
            else And(newExprs)
          }

        case Or(exprs) if exprs.contains(Const.True) =>
          Eval.now(Const.True)

        case Or(exprs) =>
          val newExprs = exprs.flatMap {
            case Or(exprs) => exprs
            case expr => List(expr)
          }.filterNot(_ == Const.False)

          Eval.now {
            if (newExprs.isEmpty) Const.False
            else if (newExprs.sizeIs == 1) newExprs.head
            else Or(newExprs)
          }

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
