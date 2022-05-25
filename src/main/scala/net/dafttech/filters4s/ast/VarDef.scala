package net.dafttech.filters4s.ast

case class VarDef(name: String, expr: Expr, body: Expr) extends Expr {

}
