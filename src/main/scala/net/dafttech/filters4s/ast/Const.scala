package net.dafttech.filters4s.ast

case class Const[T: TermType](value: T) extends Term {

}

object Const {
  object True extends Const[Boolean](true)

  object False extends Const[Boolean](false)
}
