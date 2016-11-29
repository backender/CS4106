package common

object Lang {
  // Variable name in program
  type Variable = String

  sealed abstract class Value
  case class NumValue(value: Int) extends Value
  case class BoolValue(value: Boolean) extends Value

  sealed abstract class Literal
  case class Num(literal: Int) extends Literal
  case class Bool(literal: Boolean) extends Literal
}
