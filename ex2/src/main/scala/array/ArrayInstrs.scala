package array

import common.Runtime._
import array.ArrayLang._

object ArrayInstrs {

  // Instructions
  sealed abstract class Instruction
  //  Example: x = 42
  case class AssignInstr(location: Loc, value: Expression[Loc]) extends Instruction
  //  Example: jumpif (x < 42) 4
  case class JumpIfInstr(destination: Pos, condition: Expression[Loc]) extends Instruction
  //  Example: y = x[i]
  case class ReadArrayInstr(destination: Loc, location: Loc, index: Expression[Loc]) extends Instruction
  //  Example: x[i] = 42
  case class AssignArrayInstr(location: Loc, index: Expression[Loc], value: Expression[Loc]) extends Instruction
  //  Example: x = new int[50]
  case class NewArrayInstr(location: Loc, length: Expression[Loc]) extends Instruction
  //  Example: delete a
  case class DeleteArrayInstr(location: Loc) extends Instruction
  //  Example: abort(new RuntimeException("Error"))
  case class AbortInstr(message: String) extends Instruction
}
