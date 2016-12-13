package typedfun

import common.Runtime._
import typedfun.FunLang._

object FunInstrs {
  // Instructions
  sealed abstract class Instruction
  //  Example: 4: x = 42
  case class AssignInstr(location: Loc, value: Expression[Loc]) extends Instruction
  //  Example: 5: jumpif (x < 42) 4
  case class JumpIfInstr(destination: Pos, condition: Expression[Loc]) extends Instruction
  //  Example: 6: y = x[i]
  case class ReadArrayInstr(destination: Loc, location: Loc, index: Expression[Loc]) extends Instruction
  //  Example: 7: x[i] = 42
  case class AssignArrayInstr(location: Loc, index: Expression[Loc], value: Expression[Loc]) extends Instruction
  //  Example: 8: x = new int[50]
  case class NewArrayInstr(location: Loc, length: Expression[Loc]) extends Instruction
  //  Example: 9: x = foo(2, 6)
  case class CallInstr(destination: Pos, location: Loc, frameSize: Int, args: List[Expression[Int]]) extends Instruction
  //  Example: return 42
  case class ReturnInstr(value: Expression[Int]) extends Instruction
}
