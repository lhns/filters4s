package net.dafttech.f2s.ast

import java.time.{LocalDate, LocalDateTime}

import net.dafttech.f2s.util.RichString2._

import scala.math.BigDecimal

abstract class Value {
  self =>
  type V
  val value: V

  override def hashCode(): Int = value.hashCode

  override def equals(other: scala.Any): Boolean = {
    type SameType = Value {type V = self.V}
    other match {
      case other: SameType@unchecked => value == other.value
      case _ => false
    }
  }
}

object Value {
  def apply(value: Any): Value =
    value match {
      case None => NullValue
      case Some(some) => Value(some)
      case int: Int => NumberValue(int)
      case long: Long => NumberValue(long)
      case short: Short => NumberValue(short.toInt)
      case bigDecimal: BigDecimal => NumberValue(bigDecimal)
      case bool: Boolean => BoolValue(bool)
      case string: String => StringValue(string)
      case timestamp: LocalDateTime => TimestampValue(timestamp)
      case date: LocalDate =>
        TimePatternValue(TimePattern(date.getYear.toString.padLeft(4, "0"), date.getMonthValue.toString.padLeft(2, "0"), date.getDayOfMonth.toString.padLeft(2, "0"), "*", "*", "*"))
      case timeRange: TimeRange => TimeRangeValue(timeRange)
      case timePattern: TimePattern => TimePatternValue(timePattern)
    }

  case object NullValue extends Value {
    type V = Unit
    override val value: Unit = ()
  }

  case class NumberValue(value: BigDecimal) extends Value {
    type V = BigDecimal
  }

  case class BoolValue(value: Boolean) extends Value {
    type V = Boolean
  }

  case class StringValue(value: String) extends Value {
    type V = String
  }

  case class TimestampValue(value: LocalDateTime) extends Value {
    type V = LocalDateTime
  }

  case class TimeRangeValue(value: TimeRange) extends Value {
    type V = TimeRange
  }

  case class TimePatternValue(value: TimePattern) extends Value {
    type V = TimePattern
  }

}
