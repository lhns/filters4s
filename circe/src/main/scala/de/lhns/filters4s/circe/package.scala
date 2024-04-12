package de.lhns.filters4s

package object circe {
  /*implicit val constCodec: Codec[Const[_]] = Codec.from(
    Decoder[Const[_]],
    new Encoder[Const[_]] {
      override def apply(const: Const[_]): Json =
        const match {
          case ExprType.NullType(()) => fr"NULL"
          case ExprType.StringType(string) => fr"$string"
          case ExprType.NumberType(number) => fr"$number"
          case ExprType.BoolType(boolean) => if (boolean) fr"TRUE" else fr"FALSE"
          case _ => throw new RuntimeException(s"unsupported db type: $value")
        }
    }
  )*/
}
