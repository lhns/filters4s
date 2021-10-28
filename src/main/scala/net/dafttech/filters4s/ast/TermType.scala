package net.dafttech.filters4s.ast

import cats.kernel.Eq

import java.time.{Instant, LocalDate, LocalDateTime, LocalTime}

sealed abstract class TermType[T](
                                   implicit
                                   val eq: Eq[T],
                                   val ordering: Ordering[T]
                                 )

object TermType {
  private implicit def eqFromOrdering[A](implicit ordering: Ordering[A]): Eq[A] = Eq.instance[A](ordering.equiv)

  private implicit val instantOrdering: Ordering[Instant] = Ordering.fromLessThan[Instant](_.isBefore(_))
  private implicit val localDateTimeOrdering: Ordering[LocalDateTime] = Ordering.fromLessThan[LocalDateTime](_.isBefore(_))
  private implicit val localDateOrdering: Ordering[LocalDate] = Ordering.fromLessThan[LocalDate](_.isBefore(_))
  private implicit val localTimeOrdering: Ordering[LocalTime] = Ordering.fromLessThan[LocalTime](_.isBefore(_))

  implicit object BoolType extends TermType[Boolean]

  implicit object StringType extends TermType[String]

  implicit object NumberType extends TermType[BigDecimal]

  implicit object NullType extends TermType[Unit]
}
