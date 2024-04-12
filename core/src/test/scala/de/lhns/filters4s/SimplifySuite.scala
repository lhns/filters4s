package de.lhns.filters4s

import de.lhns.filters4s.ast.Op._
import de.lhns.filters4s.ast._
import munit.FunSuite

class SimplifySuite extends FunSuite {
  test("simplify not") {
    assertEquals(Simplify(!Const.True).value, Const.False)
  }

  test("simplify const eq") {
    assertEquals(Simplify(Const(1) === Const(1)).value, Const.True)
    assertEquals(Simplify(Const(1) === Const(2)).value, Const.False)
    assertEquals(Simplify(Const(1) =!= Const(1)).value, Const.False)
    assertEquals(Simplify(Const(1) =!= Const(2)).value, Const.True)
  }

  test("simplify const gt le") {
    assertEquals(Simplify(Const(1) < Const(1)).value, Const.False)
    assertEquals(Simplify(Const(1) < Const(2)).value, Const.True)
    assertEquals(Simplify(Const(1) > Const(1)).value, Const.False)
    assertEquals(Simplify(Const(2) > Const(1)).value, Const.True)
    assertEquals(Simplify(Const(1) <= Const(1)).value, Const.True)
    assertEquals(Simplify(Const(1) <= Const(2)).value, Const.True)
    assertEquals(Simplify(Const(2) <= Const(1)).value, Const.False)
    assertEquals(Simplify(Const(1) >= Const(1)).value, Const.True)
    assertEquals(Simplify(Const(1) >= Const(2)).value, Const.False)
    assertEquals(Simplify(Const(2) >= Const(1)).value, Const.True)
  }

  test("simplify tree") {
    assertEquals(Simplify(!(Const.True && !(Const.False || Ref("test")))).value, Ref("test"))
  }

  test("resolve vars") {
    val varName = "var"
    assertEquals(Simplify(VarScope(varName, Ref("test"), Ref("test2") && VarRef(varName))).value, Ref("test2") && Ref("test"))
  }
}
