package net.dafttech.filters4s.parser

import fastparse._
import net.dafttech.filters4s.parser.OpList.{Keyword, Op, Operator}
import net.dafttech.filters4s.parser.implicits.{keyword, operator}

case class OpList(ops: List[Op]) {
  private lazy val parserArgs = {
    val keywords = ops.collect {
      case keyword@Keyword(_) => keyword
    }.map(_.name).sortBy(_.length).reverse

    val operators = ops.collect {
      case operator@Operator(_) => operator
    }.map(_.name).sortBy(_.length).reverse

    (keywords, operators)
  }

  def parser[_: P]: P[String] = {
    val (keywords, operators) = parserArgs
    operator(operators: _*) | keyword(keywords: _*)
  }
}

object OpList {
  def apply(ops: Op*): OpList = new OpList(ops.toList)

  sealed trait Op

  case class Keyword(name: String) extends Op

  case class Operator(name: String) extends Op

}
