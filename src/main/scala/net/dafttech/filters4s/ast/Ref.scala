package net.dafttech.filters4s.ast

case class Ref[T: TermType](name: String) extends Term[T] {

}
