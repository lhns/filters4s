package net.dafttech.f2s.implicits

import scala.language.implicitConversions

trait StringOpsImplicits {
  implicit def toStringOps(string: String): StringOps = new StringOps(string)
}

class StringOps(val self: String) extends AnyVal {
  def padRight(len: Int, elem: String): String = self.padTo(len, elem).mkString

  def padLeft(len: Int, elem: String): String = self.reverse.padTo(len, elem).reverse.mkString
}
