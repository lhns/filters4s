package net.dafttech.filters4s.ast

import java.time.LocalDateTime

import net.dafttech.filters4s.implicits._

case class TimeRange(start: LocalDateTime, end: LocalDateTime) {
  def inRange(dateTime: LocalDateTime): Boolean = dateTime >= start && dateTime <= end
}
