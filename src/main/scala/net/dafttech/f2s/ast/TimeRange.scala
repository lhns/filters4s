package net.dafttech.f2s.ast

import java.time.LocalDateTime

import net.dafttech.f2s.util.RichDateTime._

case class TimeRange(start: LocalDateTime, end: LocalDateTime) {
  def inRange(dateTime: LocalDateTime): Boolean = dateTime >= start && dateTime <= end
}
