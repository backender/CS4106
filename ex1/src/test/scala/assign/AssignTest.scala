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

  "A program with a single assignment" should "assign the correct boolean value" in {
    val program = Program(List(
      AssignStmt("x", Lit(Bool(false)))
    ))

    val (instr,_) = AssignCompiler.compile(program)

    AssignRuntime.run(program) shouldBe equivalentTo(instr,
      //     pc |   x
      (0, Map(0 -> null)),
      (1, Map(0 -> BoolValue(false)))
    )
  }

  "A program with multiple assignments" should "overwrite the value also with another type" in {
    val program = Program(List(
      AssignStmt("x", Lit(Num(42))),
      AssignStmt("x", Lit(Bool(true)))
    ))

    val (instr,_) = AssignCompiler.compile(program)

    //print(AssignRuntime.run(program))

    AssignRuntime.run(program) shouldBe equivalentTo(instr,
      //     pc |   x
      (0, Map(0 -> null)),
      (1, Map(0 -> NumValue(42))),
      (2, Map(0 -> BoolValue(true)))
    )
  }

   "Conditional true statement" should "trace value from first statement" in {
     val program = Program(List(
       IfStmt(
         Lit(Bool(true)),
         List(
           AssignStmt("x", Lit(Num(42)))
         ),
         List(
           AssignStmt("x", Lit(Num(666)))
         )
       )
     ))

     val (instr,_) = AssignCompiler.compile(program)

     //printTrace(AssignRuntime.run(program))

     AssignRuntime.run(program) shouldBe equivalentTo(instr,
       //     pc |   x
       (0, Map(0 -> null)),
       (1, Map(0 -> null)),
       (2, Map(0 -> NumValue(42)))
     )
   }

  "Conditional false statement" should "trace value from second statement" in {
    val program = Program(List(
      IfStmt(
        Lit(Bool(false)),
        List(
          AssignStmt("x", Lit(Num(42)))
        ),
        List(
          AssignStmt("x", Lit(Num(666)))
        )
      )
    ))

    val (instr,_) = AssignCompiler.compile(program)

    //printTrace(AssignRuntime.run(program))

    AssignRuntime.run(program) shouldBe equivalentTo(instr,
      //     pc |   x
      (0, Map(0 -> null)),
      (1, Map(0 -> null)),
      (2, Map(0 -> NumValue(666)))
    )
  }

  "Nested conditional statements" should "trace correct value" in {
    val program = Program(List(
      IfStmt(
        Lit(Bool(true)),
        List(
          IfStmt(
            Lit(Bool(false)),
            List(
              AssignStmt("x", Lit(Num(42)))
            ),
            List(
              AssignStmt("x", Lit(Num(666)))
            )
          )
        ),
        List(
          AssignStmt("x", Lit(Num(666)))
        )
      )
    ))

    val (instr,_) = AssignCompiler.compile(program)

    printTrace(AssignRuntime.run(program))

    AssignRuntime.run(program) shouldBe equivalentTo(instr,
      //     pc |   x
      (0, Map(0 -> null)),
      (1, Map(0 -> null)),
      (2, Map(0 -> null)),
      (3, Map(0 -> NumValue(666)))
    )
  }


  "While true" should "result in StackOverflowError" in {
    val program = Program(List(
      WhileStmt(
        Lit(Bool(true)),
        List(
          AssignStmt("x", Lit(Num(42)))
        )
      )
    ))

    val (instr,_) = AssignCompiler.compile(program)

    //print(AssignRuntime.run(program))

    assertThrows[StackOverflowError] {
      AssignRuntime.run(program)
    }
  }

  "While false" should "result neglect everything within statement" in {
    val program = Program(List(
      WhileStmt(
        Lit(Bool(false)),
        List(
          AssignStmt("x", Lit(Num(42)))
        )
      )
    ))

    val (instr,_) = AssignCompiler.compile(program)

    //printTrace(AssignRuntime.run(program))

    AssignRuntime.run(program) shouldBe equivalentTo(instr,
      //     pc |   x
      (0, Map(0 -> null)),
      (2, Map(0 -> null))
    )
  }

  "While assignment" should "execute once" in {
    val program = Program(List(
      AssignStmt("x", Lit(Num(0))),
      WhileStmt(
        Not(Eq(Var("x"), Lit(Num(1)))),
        List(
          AssignStmt("x", Lit(Num(1)))
        )
      )
    ))

    val (instr,_) = AssignCompiler.compile(program)

    //printTrace(AssignRuntime.run(program))

    AssignRuntime.run(program) shouldBe equivalentTo(instr,
      //     pc |   x
      (0, Map(0 -> null)),
      (1, Map(0 -> NumValue(0))),
      (2, Map(0 -> NumValue(0))),
      (3, Map(0 -> NumValue(1))),
      (5, Map(0 -> NumValue(1))) // pc jumps by 2 after last loop, see AssignRuntime:63
    )
  }

  "While assignments" should "execute twice" in {
    val program = Program(List(
      AssignStmt("x", Lit(Num(0))),
      WhileStmt(
        Not(Eq(Var("x"), Lit(Num(2)))),
        List(
          AssignStmt("x", Add(Var("x"), Lit(Num(1))))
        )
      )
    ))

    val (instr,_) = AssignCompiler.compile(program)

    //printTrace(AssignRuntime.run(program))

    AssignRuntime.run(program) shouldBe equivalentTo(instr,
      //     pc |   x
      (0, Map(0 -> null)),
      (1, Map(0 -> NumValue(0))),
      (2, Map(0 -> NumValue(0))),
      (3, Map(0 -> NumValue(1))),
      (4, Map(0 -> NumValue(1))),
      (5, Map(0 -> NumValue(2))),
      (7, Map(0 -> NumValue(2))) // pc jumps by 2 after last loop, see AssignRuntime:63
    )
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
