package net.dafttech.f2s.util

import java.time.{Instant, LocalDate, LocalDateTime, LocalTime}

import net.dafttech.f2s.util.RichDateTime._

import scala.language.implicitConversions

/**
 * Created by u016595 on 07.02.2017.
 */
class RichLocalDateTime(val self: LocalDateTime) extends AnyVal {
  def ===(other: LocalDateTime): Boolean = self.isEqual(other)

  def <(other: LocalDateTime): Boolean = self.isBefore(other)

  def >(other: LocalDateTime): Boolean = self.isAfter(other)

  def =!=(other: LocalDateTime): Boolean = !(self === other)

  def <=(other: LocalDateTime): Boolean = self === other || self < other

  def >=(other: LocalDateTime): Boolean = self === other || self > other
}

class RichLocalDate(val self: LocalDate) extends AnyVal {
  def ===(other: LocalDate): Boolean = self.isEqual(other)

  def <(other: LocalDate): Boolean = self.isBefore(other)

  def >(other: LocalDate): Boolean = self.isAfter(other)

  def =!=(other: LocalDate): Boolean = !(self === other)

  def <=(other: LocalDate): Boolean = self === other || self < other

  def >=(other: LocalDate): Boolean = self === other || self > other
}

class RichLocalTime(val self: LocalTime) extends AnyVal {
  def =!=(other: LocalTime): Boolean = self < other || self > other

  def <(other: LocalTime): Boolean = self.isBefore(other)

  def >(other: LocalTime): Boolean = self.isAfter(other)

  def ===(other: LocalTime): Boolean = !(self =!= other)

  def <=(other: LocalTime): Boolean = self === other || self < other

  def >=(other: LocalTime): Boolean = self === other || self > other
}

class RichInstant(val self: Instant) extends AnyVal {
  def =!=(other: Instant): Boolean = self < other || self > other

  def <(other: Instant): Boolean = self.isBefore(other)

  def >(other: Instant): Boolean = self.isAfter(other)

  def ===(other: Instant): Boolean = !(self =!= other)

  def <=(other: Instant): Boolean = self === other || self < other

  def >=(other: Instant): Boolean = self === other || self > other
}

object RichDateTime {
  implicit def fromLocalDateTime(localDateTime: LocalDateTime): RichLocalDateTime = new RichLocalDateTime(localDateTime)

  implicit def fromLocalDate(localDate: LocalDate): RichLocalDate = new RichLocalDate(localDate)

  implicit def fromLocalTime(localTime: LocalTime): RichLocalTime = new RichLocalTime(localTime)

  implicit def fromInstant(instant: Instant): RichInstant = new RichInstant(instant)
}
