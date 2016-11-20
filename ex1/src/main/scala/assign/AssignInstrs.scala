import common.Runtime._
import AssignLang._

object AssignInstrs {

  // Instructions
  sealed abstract class Instruction
  // Example: 4: x = 42
  case class AssignInstr(location: Loc, value: Expression[Loc]) extends Instruction

  // TODO: Add an instruction for ifjump
}
