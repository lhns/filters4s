package net.dafttech.f2s.parser

import java.time.LocalDate

import fastparse.NoWhitespace._
import fastparse._
import net.dafttech.f2s.ast.ColumnFilter.Logic.{False, True}
import net.dafttech.f2s.ast.ColumnFilter._
import net.dafttech.f2s.ast.{ColumnFilter, Table, TimePattern, Value}
import net.dafttech.f2s.parser.parserutils.ParserUtils._
import net.dafttech.f2s.parser.parserutils.implicits._

/**
 * Created by u016595 on 14.11.2016.
 */
class FilterParser(val table: Table) {
  val substitutions: Map[String, () => Value] = Map(
    "#today" -> (() => Value(LocalDate.now)),
    "#yesterday" -> (() => Value(LocalDate.now.plusDays(-1))),
    "#thismonth" -> { () =>
      val now = LocalDate.now
      Value(TimePattern.of(now.getYear.toString, now.getMonthValue.toString, "*", "*", "*", "*"))
    },
    "#lastmonth" -> { () =>
      val now = LocalDate.now.plusMonths(-1)
      Value(TimePattern.of(now.getYear.toString, now.getMonthValue.toString, "*", "*", "*", "*"))
    },
    "#thisyear" -> { () =>
      val now = LocalDate.now
      Value(TimePattern.of(now.getYear.toString, "*", "*", "*", "*", "*"))
    },
    "#lastyear" -> { () =>
      val now = LocalDate.now.plusYears(-1)
      Value(TimePattern.of(now.getYear.toString, "*", "*", "*", "*", "*"))
    }
  )

  /*def parser[_: P]: P[Either[DBError, ColumnFilter]] = LogicParser.parser.map { filter =>
    FilterCompiler.resolveColumns(filter, table)
      .map(_.transform {
        case Comp(ColRef(col), op, ConstValue(value)) if Platform() != Platform.JS =>
          for {
            key <- value.getAs[String].map(_.toLowerCase)
            substitutionF <- substitutions.get(key)
          } yield
            Comp(ColRef(col), op, ConstValue(substitutionF()))

        case _ => none
      })
      .map(table.desugarFilter)
      .map(FilterCompiler.centrifugeFilter)
  }

  val default: Either[DBError, ColumnFilter] = False.asRight[DBError]*/
}

object FilterParser {

  private object LogicParser {
    private def not[_: P] = -((operator("!") | keyword("not", "nicht")) ~ s)

    private def xor[_: P] = -(keyword("xor") ~ s)

    private def and[_: P] = -((operator("&&", "&") | keyword("und", "and")) ~ s | !or ~ s1)

    private def or[_: P] = -((operator("||", "|") | keyword("oder", "or")) ~ s)

    private def bracketExpr[_: P]: P[ColumnFilter] =
      (-operator("(") ~ s ~/ parser ~ -operator(")")) | ComparisonParser.parser

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
    def eq[_: P] = (
      operator("==", "=") |
        keyword(
          "ist", "gleich",
          "is", "equals",
          "eq"
        )
      ).map(_ => Op.Eq)

    def neq[_: P] = (
      operator("!=", "^=", "!", "<>") |
        keyword(
          "nicht gleich", "ungleich", "nicht",
          "is not", "does not equal", "unequal",
          "neq"
        )
      ).map(_ => Op.NEq)

    def le[_: P] = (operator("<") | keyword("weniger als", "vor", "less than", "le")).map(_ => Op.Le)

    def gt[_: P] = (operator(">") | keyword("mehr als", "nach", "more than", "gt")).map(_ => Op.Gt)

    def leq[_: P] = (operator("<=") | keyword("weniger gleich", "less than or equal to", "leq")).map(_ => Op.LEq)

    def geq[_: P] = (operator(">=") | keyword("mehr gleich", "greater than or equal to", "geq")).map(_ => Op.GEq)

    def like[_: P] = keyword("wie", "like").map(_ => Op.Eq)

    def timeRangeValue[_: P]: P[(Value, Value)] =
      (ValueParser.timestampValue ~ -operator("-") ~ ValueParser.timestampValue).map {
        case (start, end) => (
          /*Value(ValueType.timestamp.getMinValue(start).get),
          Value(ValueType.timestamp.getMaxValue(end).get)*/
          (start, end)
          )
      }

    def timeRange[_: P]: P[Logic] =
      (identifier ~ -(eq | like) ~ s ~ (quote ~ s ~ timeRangeValue ~ s ~ quote | timeRangeValue)).map {
        case (name, range) =>
          Comp(ColName(name), Op.GEq, ConstValue(range._1)) &&
            Comp(ColName(name), Op.LEq, ConstValue(range._2))
      }

    private def operation[_: P]: P[ColumnFilter] = (identifier ~ (eq | neq | leq | geq | le | gt | like) ~ s ~/ ValueParser.parser).map {
      case (name, op, value) => Comp(ColName(name), op, ConstValue(value))
    }

    private def const[_: P]: P[Logic] =
      keyword("true").map[Logic](_ => True) | keyword("false").map(_ => False)

    private def boolCheck[_: P]: P[Comp] = identifier.map(name => Comp(ColName(name), Op.Eq, ConstValue(Value(true))))

    def parser[_: P]: P[ColumnFilter] = timeRange | operation | const | boolCheck
  }

}
