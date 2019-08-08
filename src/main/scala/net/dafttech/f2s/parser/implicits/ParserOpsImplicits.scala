package net.dafttech.f2s.parser.implicits

import fastparse._

import scala.language.implicitConversions

trait ParserOpsImplicits {
  implicit def toParserOps[A](parser: P[A]): ParserOps[A] = new ParserOps(parser)
}

class ParserOps[A](val self: P[A]) extends AnyVal {
  def unary_- : P[Unit] = self.map(_ => ())
}
