package net.dafttech.filters4s.parser.implicits

import cats.syntax.either._
import fastparse.Parsed

import scala.language.implicitConversions
import scala.util.Try

trait ParsedOpsImplicits {
  implicit def toParsedOps[A](parsed: Parsed[A]): ParsedOps[A] = new ParsedOps(parsed)
}

class ParsedOps[A](val self: Parsed[A]) extends AnyVal {
  def toEither: Either[Parsed.Failure, Parsed.Success[A]] = self match {
    case success: Parsed.Success[A] => Either.right(success)
    case failure: Parsed.Failure => Either.left(failure)
  }

  def toOption: Option[Parsed.Success[A]] = toEither.toOption

  def toTry: Try[Parsed.Success[A]] = Try(self.get)
}
