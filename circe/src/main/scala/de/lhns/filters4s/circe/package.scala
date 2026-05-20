package de.lhns.filters4s

import de.lhns.filters4s.ast.{Const, ExprType}
import io.circe.generic.semiauto.deriveCodec
import io.circe.{Codec, Encoder, Json}

package object circe {
  implicit val constCodec: Codec[Const[_]] = {
    case class ConstJson(value: Json, `type`: String)
    val constJsonCodec: Codec[ConstJson] = deriveCodec

    Codec.from(
      constJsonCodec.map { json =>
        val value = json.`type` match {
          case ExprType.NullType.name => Const((), ExprType.NullType)
          case ExprType.StringType.name => Const(json.value.asString.get, ExprType.StringType)
        }
      },
      new Encoder[Const[_]] {
        override def apply(const: Const[_]): Json =
          const match {
            case ExprType.NullType(()) => Json.Null
            case ExprType.StringType(string) => Json.fromString(string.value)
            case ExprType.NumberType(number) => Json.fromBigDecimal(number.value)
            case ExprType.BoolType(boolean) => if (boolean.value) Json.True else Json.False
            case _ => throw new RuntimeException(s"unsupported json type: ${const}")
          }
      }
    )
  }
}
