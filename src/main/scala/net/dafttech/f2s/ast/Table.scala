package net.dafttech.f2s.ast

/**
 * Created by u016595 on 11.11.2016.
 */
abstract class Table(val name: String) {
  def columns: List[Column[_]]

  lazy val columnMap: Map[String, Column[_]] =
    columns.map(column => column.name -> column).toMap

  def desugarFilter(filter: ColumnFilter): ColumnFilter = filter

  override def toString: String = s"Table($name)"
}
