package net.dafttech.f2s.util

import scala.language.implicitConversions

class RichString2(val self: String) extends AnyVal {
  def splitQuoted: List[String] = {
    val quotes: List[String] = self.split("\"")
      .foldLeft[List[String]](Nil) { (last, e) =>
        last.lastOption match {
          case Some(prev) if prev.endsWith("\\") =>
            last.dropRight(1) :+ s"""${prev.dropRight(1)}"$e"""
          case _ =>
            last :+ e
        }
      }

    quotes
      .foldLeft[(List[String], Boolean)](Nil, true) { (last, e) =>
        last match {
          case (part, true) =>
            (part ++ e.split(" "), false)

          case (part, false) =>
            (part :+ e, true)
        }
      }._1
      .filter(_.nonEmpty)
  }

  def padRight(len: Int, elem: String): String = self.padTo(len, elem).mkString

  def padLeft(len: Int, elem: String): String = self.reverse.padTo(len, elem).reverse.mkString
}

object RichString2 {
  implicit def fromString(string: String): RichString2 = new RichString2(string)
}
