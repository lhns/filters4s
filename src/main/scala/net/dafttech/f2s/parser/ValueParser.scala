package net.dafttech.f2s.parser

import fastparse.NoWhitespace._
import fastparse._
import net.dafttech.f2s.ast.Value._
import net.dafttech.f2s.ast.{TimePattern, Value}
import net.dafttech.f2s.parser.parserutils.ParserUtils._

object ValueParser {
  def nullValue[_: P]: P[NullValue.type] = IgnoreCase("null").map(_ => NullValue).opaque("null")

  def boolValue[_: P]: P[BoolValue] = (
    StringInIgnoreCase("true", "yes", "on", "wahr", "ja", "an", "y", "j").map(_ => true) |
      StringInIgnoreCase("false", "no", "off", "falsch", "nein", "aus", "n").map(_ => false)
    ).map(BoolValue).opaque("bool")

  private def ddmmyyyyDateParser[_: P]: P[(String, String, String)] =
    ((digit | "*").rep(min = 1, max = 2).! ~ "." ~
      (digit | "*").rep(min = 1, max = 2).! ~ "." ~
      (digit | "*").rep(min = 1, max = 4).!).map {
      case (day, month, year) => (year, month, day)
    }.opaque("dd.MM.yyyy")

  private def yyyymmddDateParser[_: P]: P[(String, String, String)] =
    ((digit | "*").rep(min = 3, max = 4).! ~ "." ~
      (digit | "*").rep(min = 1, max = 2).! ~ "." ~
      (digit | "*").rep(min = 1, max = 2).!)
      .opaque("yyyy.MM.dd")

  private def dateParser[_: P]: P[(String, String, String)] = (ddmmyyyyDateParser | yyyymmddDateParser).opaque("date")

  private def timeParser[_: P]: P[(String, String, String)] =
    (digit | "*").rep(min = 1, max = 2).! ~ ":" ~
      (digit | "*").rep(min = 1, max = 2).! ~
      (":" ~ (digit | "*").rep(min = 1, max = 2).!).?
        .map(second => second.getOrElse("*")).opaque("time")

  private def timestampParser[_: P]: P[(String, String, String, String, String, String)] = {
    def dateTime =
      (dateParser ~ s1 ~ timeParser).map(e => (e._1, e._2, e._3, e._4._1, e._4._2, e._4._3))

    def date =
      (dateParser ~ (s1 ~ "*").?).map(e => (e._1, e._2, e._3, "*", "*", "*"))

    def time =
      ((s1 ~ "*").? ~ timeParser).map(e => ("*", "*", "*", e._1, e._2, e._3))

    (dateTime | date | time).opaque("date or time")
  }

  def timestampValue[_: P]: P[TimePatternValue] = timestampParser.map {
    case (year, month, day, hour, minute, second) =>
      val pattern = TimePattern.of(year, month, day, hour, minute, second)
      TimePatternValue(pattern)
  }

  def yyyymmddhhmmssValue[_: P]: P[TimePatternValue] =
    (digit.rep(exactly = 4).! ~ digit.rep(exactly = 2).! ~ digit.rep(exactly = 2).! ~
      digit.rep(exactly = 2).! ~ digit.rep(exactly = 2).! ~ digit.rep(exactly = 2).!).map {
      case (year, month, day, hour, minute, second) =>
        val pattern = TimePattern.of(year, month, day, hour, minute, second)
        TimePatternValue(pattern)
    }.opaque("yyyyMMddHHmmss")

  def numberValue[_: P]: P[NumberValue] = number.map(NumberValue)

  def stringValue[_: P]: P[StringValue] = text.map(StringValue)

  def parser[_: P]: P[Value] = any(Seq( // FIXME: This is important (best parser first)
    () => ValueParser.nullValue,
    () => ValueParser.boolValue,
    () => ValueParser.timestampValue,
    () => ValueParser.numberValue,
    () => ValueParser.stringValue
  )).opaque("value")
}
