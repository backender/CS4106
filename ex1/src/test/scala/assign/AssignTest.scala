import org.scalatest._
import common.Lang._
import common.Runtime._
import AssignLang._
import AssignInstrs._
import AssignCompiler._
import AssignRuntime._

class AssignTest extends FlatSpec with Matchers {

  "The empty program" should "produce an empty trace" in {
    AssignRuntime.run(program()) shouldBe List(
      State(List(), 0, Heap.empty)
    )
  }

  "A program with a single assignment" should "assign the correct value" in {
    val program = Program(List(
      AssignStmt("x", Lit(Num(42)))
    ))

    val (instr,_) = AssignCompiler.compile(program)

    AssignRuntime.run(program) shouldBe equivalentTo(instr,
//     pc |   x
      (0, Map(0 -> null)),
      (1, Map(0 -> NumValue(42)))
    )
  }


  "A program with multiple assignments" should "assign the correct values to each" in {
    val program = Program(List(
      AssignStmt("x", Lit(Num(42))),
      AssignStmt("y", Lit(Num(666)))
    ))

    val (instr,_) = AssignCompiler.compile(program)

    AssignRuntime.run(program) shouldBe equivalentTo(instr,
//     pc |   x                | y
      (0, Map(0 -> null,         1 -> null)),
      (1, Map(0 -> NumValue(42), 1 -> null)),
      (2, Map(0 -> NumValue(42), 1 -> NumValue(666)))
    )
  }

  "A program with multiple assignments" should "overwrite the value" in {
    val program = Program(List(
      AssignStmt("x", Lit(Num(42))),
      AssignStmt("x", Lit(Num(666)))
    ))

    val (instr,_) = AssignCompiler.compile(program)

    //print(AssignRuntime.run(program))

    AssignRuntime.run(program) shouldBe equivalentTo(instr,
//     pc |   x
      (0, Map(0 -> null)),
      (1, Map(0 -> NumValue(42))),
      (2, Map(0 -> NumValue(666)))
    )
  }

  // TODO: Add your own tests here
  "Task 1" should "print conditional traces" in {
    val program = Program(List(
      IfStmt(
        Lit(Bool(true)),
        List(
          AssignStmt("x", Lit(Num(42)))
        ),
        List(
          AssignStmt("x", Lit(Num(66)))
        )
      )
    ))

    val trace = AssignRuntime.run(program)
    print(trace)

    1 shouldBe 1
  }

  def program(statements: Statement*) = Program(statements.toList)

  def equivalentTo(instr: List[Instruction], l: (Int, Map[Loc, Value])*): List[State] = {
    l.map {
      case (pc, mem) => {
        val newMem = Heap.empty.alloc(mem.size)._1
        State(instr, pc, mem.foldLeft(newMem) { case (m, (l, v)) => m.store(l, v) })
      }
    }.toList
  }

}
