package net.dafttech.filters4s.ast

abstract class Table(val name: String) {
  def columns: List[Column[_]]

  lazy val columnMap: Map[String, Column[_]] =
    columns.map(column => column.name -> column).toMap

  override def toString: String = s"Table($name)"
}
