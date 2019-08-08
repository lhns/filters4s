package net.dafttech.f2s.implicits

import java.time.{Instant, LocalDate, LocalDateTime, LocalTime}

import cats.kernel.Eq
import cats.syntax.EqSyntax

trait OrderingImplicits extends Ordering.ExtraImplicits with EqSyntax {
  implicit def eqFromOrdering[A](implicit ordering: Ordering[A]): Eq[A] = Eq.instance[A](ordering.equiv)

  implicit val instantOrdering: Ordering[Instant] = Ordering.fromLessThan[Instant](_.isBefore(_))
  implicit val localDateTimeOrdering: Ordering[LocalDateTime] = Ordering.fromLessThan[LocalDateTime](_.isBefore(_))
  implicit val localDateOrdering: Ordering[LocalDate] = Ordering.fromLessThan[LocalDate](_.isBefore(_))
  implicit val localTimeOrdering: Ordering[LocalTime] = Ordering.fromLessThan[LocalTime](_.isBefore(_))
}
