import common.Runtime._
import AssignLang._

object AssignInstrs {

  // Instructions
  sealed abstract class Instruction
  // Example: 4: x = 42
  case class AssignInstr(location: Loc, value: Expression[Loc]) extends Instruction

  // TODO: Add an instruction for ifjump
  case class IfStmtInstr(value: Expression[Loc], ifInstrs: List[Instruction], elseInstrs: List[Instruction]) extends Instruction

  case class WhileStmtInstr(value: Expression[Loc], branchInstr: List[Instruction]) extends Instruction
}
