package de.lhns.filters4s.doobie

import de.lhns.filters4s.ast.Op._
import de.lhns.filters4s.ast._
import munit.FunSuite

class DoobieSuite extends FunSuite {
  test("doobie") {
    println(
      DoobieCompiler.compile(!(Const.True && !(Const.False || Ref("test")))).value.toString()
    )
  }
}
