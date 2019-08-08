package net.dafttech.f2s.ast

import java.time.temporal.TemporalAdjusters
import java.time.{LocalDate, LocalDateTime}

import cats.syntax.option._
import net.dafttech.f2s.util.RichString2._

case class TimePattern(year: String, month: String, day: String, hour: String, minute: String, second: String) {
  def toTimeRange: Option[TimeRange] = {
    val patternString = s"$year$month$day$hour$minute$second"
    if (!patternString.reverse.dropWhile(_ == '*').reverse.contains('*')) {
      def min(string: String, length: Int, min: Int = 0): Int =
        Math.max((if (string.contains("*")) string.replaceAllLiterally("*", "").padRight(length, "0") else string).toInt, min)

      def max(string: String, length: Int, max: Int = Integer.MAX_VALUE): Int =
        Math.min((if (string.contains("*")) string.replaceAllLiterally("*", "").padRight(length, "9") else string).toInt, max)

      val minDateTime = LocalDateTime.of(min(year, 4), min(month, 2, 1), min(day, 2, 1), min(hour, 2), min(minute, 2), min(second, 2))
      val maxDateTime = LocalDateTime.of(max(year, 4), max(month, 2, 12), 1, max(hour, 2, 23), max(minute, 2, 59), max(second, 2, 59))
      val lastDayOfMonth = maxDateTime.`with`(TemporalAdjusters.lastDayOfMonth).getDayOfMonth
      val maxDateTime2 = maxDateTime.withDayOfMonth(max(day, 2, lastDayOfMonth))

      TimeRange(minDateTime, maxDateTime2).some
    } else
      none
  }

  def likeClause: String = s"$year-$month-$day-$hour.$minute.$second.*"

  override def toString: String = s"$day.$month.$year $hour:$minute:$second"
}

object TimePattern {
  def of(year: String, month: String, day: String, hour: String, minute: String, second: String): TimePattern = {
    val currentYear = LocalDate.now.getYear.toString

    val newYear = if (year.contains("*")) year else s"${currentYear.take(currentYear.length - year.length)}$year"
    val newMonth = if (month.contains("*")) month else Math.max(month.toInt, 1).toString.padLeft(2, "0")
    val newDay = if (day.contains("*")) day else Math.max(day.toInt, 1).toString.padLeft(2, "0")
    val newHour = if (hour.contains("*")) hour else hour.padLeft(2, "0")
    val newMinute = if (minute.contains("*")) minute else minute.padLeft(2, "0")
    val newSecond = if (second.contains("*")) second else second.padLeft(2, "0")

    new TimePattern(newYear, newMonth, newDay, newHour, newMinute, newSecond)
  }
}
