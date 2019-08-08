package net.dafttech.f2s.ast

import net.dafttech.f2s.ast.ColumnFilter.{ColRef, Comp, ConstValue, Op}

/**
 * Created by u016595 on 11.11.2016.
 */
case class Column[V] private(name: String, valueType: String) {
  def ===(value: Value): Comp = Comp(ColRef(this), Op.Eq, ConstValue(value))

  def =!=(value: Value): Comp = Comp(ColRef(this), Op.NEq, ConstValue(value))

  def <(value: Value): Comp = Comp(ColRef(this), Op.Le, ConstValue(value))

  def >(value: Value): Comp = Comp(ColRef(this), Op.Gt, ConstValue(value))

  def <=(value: Value): Comp = Comp(ColRef(this), Op.LEq, ConstValue(value))

  def >=(value: Value): Comp = Comp(ColRef(this), Op.GEq, ConstValue(value))

  def like(value: Value): Comp = this === value //ColumnFilter.ColumnComp(this, Operator.Like, value)

  private val groupSize = 30

  //def withVirtual(virtual: Boolean): Column[V] = Column(name, virtual)(valueType)

  //def virtual: Boolean = _virtual.value

  def in(values: Seq[Value]): ColumnFilter = {
    val comps = values.map(this === _)

    def groupRec(filters: Seq[ColumnFilter]): ColumnFilter = filters match {
      case Seq() => ColumnFilter.Logic.False
      case Seq(head) => head
      case Seq(head, tail@_*) if tail.size < groupSize =>
        tail.foldLeft(head)((acc, e) => acc || e)
      case seq =>
        val (left, right) = seq.splitAt(seq.size / 2)
        groupRec(left) || groupRec(right)
    }

    groupRec(comps)
  }

  //val rep: Rep[V]
  //def ===(column: Column[V]) = rep === column.rep
}
