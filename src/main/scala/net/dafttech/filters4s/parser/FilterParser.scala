package net.dafttech.filters4s.parser

import fastparse.NoWhitespace._
import fastparse._
import net.dafttech.filters4s.ast.ColumnFilter.Logic.{False, True}
import net.dafttech.filters4s.ast.ColumnFilter._
import net.dafttech.filters4s.ast.{ColumnFilter, Value}
import net.dafttech.filters4s.parser.OpList.{Keyword, Operator}
import net.dafttech.filters4s.parser.implicits._

class FilterParser(notOps: OpList,
                   xorOps: OpList,
                   andOps: OpList,
                   orOps: OpList,
                   eqOps: OpList,
                   neqOps: OpList,
                   leOps: OpList,
                   gtOps: OpList,
                   leqOps: OpList,
                   geqOps: OpList,
                   trueOps: OpList,
                   falseOps: OpList,
                  ) {

  private object LogicParser {
    private def not[_: P] = -(notOps.parser ~ s)

    private def xor[_: P] = -(xorOps.parser ~ s)

    private def and[_: P] = -(andOps.parser ~ s | !or ~ s1)

    private def or[_: P] = -(orOps.parser ~ s)

    private def bracketExpr[_: P]: P[ColumnFilter] =
      (-operator("(") ~/ s ~/ parser ~ -operator(")")) | ComparisonParser.parser

    private def notExpr[_: P]: P[ColumnFilter] = (not ~/ notExpr).map { a =>
      !a
    } | bracketExpr

    private def xorExpr[_: P]: P[ColumnFilter] = (notExpr ~ (xor ~/ parser).?).map {
      case (a, None) => a
      case (a, Some(b)) => a xor b
    }

    private def andExpr[_: P]: P[ColumnFilter] = (xorExpr ~ (and ~ parser).?).map {
      case (a, None) => a
      case (a, Some(b)) => a && b
    }

    private def orExpr[_: P]: P[ColumnFilter] = (andExpr ~ (or ~/ parser).?).map {
      case (a, None) => a
      case (a, Some(b)) => a || b
    }

    def parser[_: P]: P[ColumnFilter] = orExpr
  }

  object ComparisonParser {
    def eq[_: P] = eqOps.parser.map(_ => Op.Eq)

    def neq[_: P] = neqOps.parser.map(_ => Op.NEq)

    def le[_: P] = leOps.parser.map(_ => Op.Le)

    def gt[_: P] = gtOps.parser.map(_ => Op.Gt)

    def leq[_: P] = leqOps.parser.map(_ => Op.LEq)

    def geq[_: P] = geqOps.parser.map(_ => Op.GEq)

    def timeRangeValue[_: P]: P[(Value, Value)] =
      (ValueParser.timestampValue ~ -operator("-") ~ ValueParser.timestampValue).map {
        case (start, end) => (
          /*Value(ValueType.timestamp.getMinValue(start).get),
          Value(ValueType.timestamp.getMaxValue(end).get)*/
          (start, end) // TODO /\ recover this
          )
      }

    def timeRange[_: P]: P[Logic] =
      (identifier ~ -(eq) ~ s ~ (quote ~ s ~ timeRangeValue ~ s ~ quote | timeRangeValue)).map {
        case (name, range) =>
          Comp(ColName(name), Op.GEq, ConstValue(range._1)) &&
            Comp(ColName(name), Op.LEq, ConstValue(range._2))
      }

    private def operation[_: P]: P[ColumnFilter] = (identifier ~ (eq | neq | leq | geq | le | gt) ~ s ~/ ValueParser.parser).map {
      case (name, op, value) => Comp(ColName(name), op, ConstValue(value))
    }

    private def const[_: P]: P[Logic] =
      trueOps.parser.map[Logic](_ => True) | falseOps.parser.map(_ => False)

    private def boolCheck[_: P]: P[Comp] = identifier.map(name => Comp(ColName(name), Op.Eq, ConstValue(Value(true))))

    def parser[_: P]: P[ColumnFilter] = timeRange | operation | const | boolCheck
  }

}

object FilterParser {
  val defaultNotOps = OpList(
    Operator("!"),
    Keyword("not"),
    Keyword("nicht"),
  )

  val defaultXorOps = OpList(
    Keyword("xor"),
  )

  val defaultAndOps = OpList(
    Operator("&"),
    Operator("&&"),
    Keyword("and"),
    Keyword("und"),
  )

  val defaultOrOps = OpList(
    Operator("|"),
    Operator("||"),
    Keyword("or"),
    Keyword("oder"),
  )

  val defaultEqOps = OpList(
    Operator("="),
    Operator("=="),
    Keyword("eq"),
    Keyword("equals"),
    Keyword("is"),
    Keyword("like"),
    Keyword("ist"),
    Keyword("gleich"),
    Keyword("wie"),
  )

  val defaultNeqOps = OpList(
    Operator("!="),
    Operator("^="),
    Operator("~="),
    Operator("!"),
    Operator("<>"),
    Keyword("neq"),
    Keyword("unequal"),
    Keyword("not equals"),
    Keyword("does not equal"),
    Keyword("is not"),
    Keyword("nicht"),
    Keyword("ungleich"),
    Keyword("nicht gleich"),
  )

  val defaultLeOps = OpList(
    Operator("<"),
    Keyword("le"),
    Keyword("less than"),
    Keyword("weniger als"),
    Keyword("vor"),
  )

  val defaultGtOps = OpList(
    Operator(">"),
    Keyword("gt"),
    Keyword("greater than"),
    Keyword("more than"),
    Keyword("mehr als"),
    Keyword("nach"),
  )

  val defaultLeqOps = OpList(
    Operator("<="),
    Keyword("leq"),
    Keyword("less or equal"),
    Keyword("weniger gleich"),
  )

  val defaultGeqOps = OpList(
    Operator(">="),
    Keyword("geq"),
    Keyword("greater or equal"),
    Keyword("more or equal"),
    Keyword("mehr gleich"),
  )

  val defaultTrueOps = OpList(Keyword("true"))
  val defaultFalseOps = OpList(Keyword("false"))
}
