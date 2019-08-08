package net.dafttech.f2s.parser.parserutils

import cats.syntax.either._
import cats.syntax.option._
import fastparse._

import scala.util.Try

object implicits {

  implicit class RichParser[T](val self: P[T]) extends AnyVal {
    def unary_- : P[Unit] = self.map(_ => ())
  }

  implicit class RichParsed[T](val self: Parsed[T]) extends AnyVal {
    def toTry: Try[T] = Try(self.get.value)

    def toOption: Option[T] = if (self.isSuccess) self.get.value.some else none

    def toEither: Either[Exception, T] = toTry.toEither.leftMap(_.asInstanceOf[Exception])

    def getResult: T = self.get.value
  }

}
