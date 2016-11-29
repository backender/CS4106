package array

import org.scalatest._
import common.Lang._
import common.Runtime._
import array.ArrayLang._
import array.ArrayInstrs._
import array.ArrayRuntime._

class ArrayTest extends FlatSpec with Matchers {

  "The normal array compiler" should "compile the empty program to instructions producing an empty trace" in {
    val compiler = new ArrayCompiler()
    ArrayRuntime.run(compiler,program()) shouldBe theTrace(List(),
      (0, Vector())
    )
  }

  it should "when assigning and retrieving from an array, return the same result" in {
    val compiler = new ArrayCompiler()

    val prog: Program = program(
      AssignStmt("len", Lit(Num(3))),
      NewArrayStmt("arr", Var("len")),
      AssignArrayStmt("arr", Lit(Num(1)), Lit(Num(42))),
      ReadArrayStmt("res", "arr", Lit(Num(1)))
    )

    val (instrs, _) = compiler.compile(prog)
    instrs shouldBe List(
      AssignInstr(0, Lit(Num(3))),
      NewArrayInstr(1, Var(0)),
      AssignArrayInstr(1, Lit(Num(1)), Lit(Num(42))),
      ReadArrayInstr(2, 1, Lit(Num(1)))
    )

    ArrayRuntime.run(compiler, prog) shouldBe theTrace(instrs,
      //         len        | arr        | res         | arrlen     | arr[0] | arr[1]      | arr[2]
      (0, Vector(null,        null,        null)),
      (1, Vector(NumValue(3), null,        null)),
      (2, Vector(NumValue(3), NumValue(3), null,         NumValue(3), null,    null,         null)),
      (3, Vector(NumValue(3), NumValue(3), null,         NumValue(3), null,    NumValue(42), null)),
      (4, Vector(NumValue(3), NumValue(3), NumValue(42), NumValue(3), null,    NumValue(42), null))
    )
  }

  it should "when freeing an array make the free space available for allocation again" in {
    val compiler = new ArrayCompiler()

    val prog: Program = program(
      NewArrayStmt("x", Lit(Num(3))),
      AssignArrayStmt("x", Lit(Num(1)), Lit(Num(42))),
      DeleteArrayStmt("x"),
      NewArrayStmt("y", Lit(Num(3))),
      AssignArrayStmt("y", Lit(Num(1)), Lit(Num(43)))
    )

    val (instrs, _) = compiler.compile(prog)
    ArrayRuntime.run(compiler, prog) shouldBe theTrace(instrs,
      //         x          | y          | xlen       | x[0] | x[1]        | x[2]
      (0, Vector(null,        null)),
      (1, Vector(NumValue(2), null,        NumValue(3), null,  null,         null)),
      (2, Vector(NumValue(2), null,        NumValue(3), null,  NumValue(42), null)),
      (3, Vector(NumValue(2), null)),
      (4, Vector(NumValue(2), NumValue(2), NumValue(3), null,  NumValue(42), null)),
      (5, Vector(NumValue(2), NumValue(2), NumValue(3), null,  NumValue(43), null))
    )
  }

  // TODO: For task 2, add three tests here

  it should "leak Memory if dynamically allocated memory is not referred to by any variable" in {
    val compiler = new ArrayCompiler()
    val prog: Program = program(
      NewArrayStmt("x", Lit(Num(2))),
      AssignArrayStmt("x", Lit(Num(1)), Lit(Num(42))),
      AssignStmt("x", Lit(Num(666)))
    )

    val (instrs, _) = compiler.compile(prog)

    val trace = ArrayRuntime.run(compiler,prog)
    trace shouldBe theTrace(instrs,
      //         x              | xlen       | x[0] | x[1]
      (0, Vector(null)),
      (1, Vector(NumValue(1),   NumValue(2), null,    null)),
      (2, Vector(NumValue(1),   NumValue(2), null,    NumValue(42))),
      (3, Vector(NumValue(666), NumValue(2), null,    NumValue(42))) // contents of previously assigned arr [0, 42] is still in heap
    )
  }

  it should "cause unpredictable effects when accessing freed memory" in {
    val compiler = new ArrayCompiler()
    val prog: Program = program(
      NewArrayStmt("x", Lit(Num(2))),
      AssignArrayStmt("x", Lit(Num(1)), Lit(Num(42))),
      DeleteArrayStmt("x"),
      // Let's try to fetch "freed" memory resource by simulating y as the previous x
      NewArrayStmt("y", Lit(Num(2))),
      ReadArrayStmt("x", "y", Lit(Num(1)))
    )

    val (instrs, _) = compiler.compile(prog)
    ArrayRuntime.run(compiler, prog) shouldBe theTrace(instrs,
    //         x          | y          | xlen       | x[0] | x[1]
    (0, Vector(null,        null)),
    (1, Vector(NumValue(2), null,        NumValue(2), null,  null        )),
    (2, Vector(NumValue(2), null,        NumValue(2), null,  NumValue(42))),
    (3, Vector(NumValue(2), null)),
    (4, Vector(NumValue(2), NumValue(2), NumValue(2), null,  NumValue(42))),
    (5, Vector(NumValue(42), NumValue(2), NumValue(2), null,  NumValue(42))) // x becomes 42 via y array
    )
  }

  it should "Double frees: Freeing twice can cause that the freed memory is allocated twice afterwards." in {
    val compiler = new ArrayCompiler()
    val prog: Program = program(
      NewArrayStmt("x", Lit(Num(2))),
      AssignArrayStmt("x", Lit(Num(1)), Lit(Num(42))),
      DeleteArrayStmt("x"),
      NewArrayStmt("y", Lit(Num(2))),
      //AssignArrayStmt("y", Lit(Num(1)), Lit(Num(42))),
      DeleteArrayStmt("x")
    )

    val (instrs, _) = compiler.compile(prog)

    val trace = ArrayRuntime.run(compiler,prog)
    //println(trace)
    trace shouldBe theTrace(instrs,
      //         x          | y          | xlen       | x[0] | x[1]
    (0, Vector(null,        null)),
    (1, Vector(NumValue(2), null,        NumValue(2), null,  null        )),
    (2, Vector(NumValue(2), null,        NumValue(2), null,  NumValue(42))),
    (3, Vector(NumValue(2), null)),
      //         x          | y          | ylen       | y[0] | y[1]
    (4, Vector(NumValue(2), NumValue(2), NumValue(2), null,  null        )),
    (5, Vector(NumValue(2), NumValue(2), NumValue(2), null,  NumValue(42))),
    (6, Vector(NumValue(2), NumValue(2)))
    )
  }


  "The safe array compiler" should "not reject a valid program" in {
    val compiler = new SafeArrayCompiler()

    val prog: Program = program(
      NewArrayStmt("x", Lit(Num(3))),
      AssignArrayStmt("x", Lit(Num(1)), Lit(Num(42))),
      DeleteArrayStmt("x"),
      NewArrayStmt("y", Lit(Num(3))),
      AssignArrayStmt("y", Lit(Num(1)), Lit(Num(43)))
    )

    val (instrs, _) = compiler.compile(prog)

    // TODO: Fill in your trace
    ArrayRuntime.run(compiler,prog) shouldBe theTrace(instrs)
  }

  // TODO: For task 4, add 2 tests here starting with

  "The safe array runtime" should "not reject a valid program" in {
    val compiler = new ArrayCompiler()
    val prog: Program = program(
      NewArrayStmt("x", Lit(Num(3))),
      AssignArrayStmt("x", Lit(Num(1)), Lit(Num(42))),
      DeleteArrayStmt("x"),
      NewArrayStmt("y", Lit(Num(3))),
      AssignArrayStmt("y", Lit(Num(1)), Lit(Num(43)))
    )

    val (instrs, _) = compiler.compile(prog)

    // TODO: Fill in your trace
    SafeArrayRuntime.run(compiler,prog) shouldBe theSafeTrace(instrs)
  }

  // TODO: For task 6, add 2 tests here

  def theTrace(instr: List[Instruction], l: (Int, Vector[Value])*): List[ArrayRuntime.State] = {
    l.map { case (pc, mem) => ArrayRuntime.State(instr, pc, Heap(mem,List())) }.toList
  }

  def theSafeTrace(instr: List[Instruction], l: (Int, Vector[Value], Set[Loc])*): List[SafeArrayRuntime.State] = {
    l.map { case (pc, mem, free) => SafeArrayRuntime.State(instr, pc, Heap(mem, List()), free) }.toList
  }
}
