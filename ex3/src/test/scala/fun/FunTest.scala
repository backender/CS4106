import org.scalatest._

import common.Lang._
import common.Runtime._

import FunLang._
import FunInstrs._
import FunCompiler._
import FunRuntime._

class FunTest extends FlatSpec with Matchers {

  "The empty program" should "produce an empty trace" in {
    val prog = program(List())
    FunRuntime.run(prog) shouldBe theTrace(List(),
      (Registers(0, 0, 0, 0), Vector())
    )
  }

  "The main function" should "execute properly" in {
    val prog = program(
      List(
        AssignStmt("x", Lit(Num(42)))
      )
    )

    val instrs = List(
      AssignInstr(0, Lit(Num(42)))
    )
    FunCompiler.compile(prog)._1 shouldBe instrs

    FunRuntime.run(prog) shouldBe theTrace(instrs,
      (Registers(0, 0, 0, 0), Vector(null)),
      (Registers(1, 0, 0, 0), Vector(NumValue(42)))
    )
  }

  it should "be able to call other functions" in {
    val prog = program(
      List(
        CallStmt("x", "foo", List(Lit(Num(2)), Lit(Num(6))))
      ),
      Function("foo", List("a", "b"), List(
        AssignStmt("c", Mul(Var("a"), Var("b"))),
        ReturnStmt(Var("c"))
      ))
    )

    val instrs = List(
      AssignInstr(2, Mul(Var(0), Var(1))),
      ReturnInstr(Var(2)),
      CallInstr(0, 0, 3, List(Lit(Num(2)), Lit(Num(6))))
    )
    FunCompiler.compile(prog)._1 shouldBe instrs

    FunRuntime.run(prog) shouldBe theTrace(instrs,
      (Registers(2, 0, 0, 0), Vector(null)),
      (Registers(0, 5, 3, 0), Vector(null, 2, 0, 0, 0, 2, 6, null)),
      (Registers(1, 5, 3, 0), Vector(null, 2, 0, 0, 0, 2, 6, 12)),
      (Registers(3, 0, 0, 0), Vector(12))
    )
  }

  "A non-main function" should "be able to call itself recursively" in {
    val prog = program(
      List(
        CallStmt("x", "foo", List(Lit(Num(2))))
      ),
      Function("foo", List("a"), List(
        WhileStmt(Gt(Var("a"), Lit(Num(0))), List(
          CallStmt("a", "foo", List(Sub(Var("a"), Lit(Num(1)))))
        )),
        ReturnStmt(Var("a"))
      ))
    )

    val instrs = List(
      JumpIfInstr(3, Not(Gt(Var(0), Lit(Num(0))))),
      CallInstr(0, 0, 1, List(Sub(Var(0), Lit(Num(1))))),
      JumpIfInstr(0, Lit(Bool(true))),
      ReturnInstr(Var(0)),
      CallInstr(0, 0, 1, List(Lit(Num(2))))
    )
    FunCompiler.compile(prog)._1 shouldBe instrs

    FunRuntime.run(prog) shouldBe theTrace(instrs,
      (Registers(4, 0, 0, 0), Vector(null)),
      // enter: foo(2)
      (Registers(0, 5, 1, 0), Vector(null, 4, 0, 0, 0, 2)),
      (Registers(1, 5, 1, 0), Vector(null, 4, 0, 0, 0, 2)),
      // enter: foo(1)
      (Registers(0, 10, 1, 0), Vector(null, 4, 0, 0, 0, 2, 1, 5, 1, 0, 1)),
      (Registers(1, 10, 1, 0), Vector(null, 4, 0, 0, 0, 2, 1, 5, 1, 0, 1)),
      // enter: foo(0)
      (Registers(0, 15, 1, 0), Vector(null, 4, 0, 0, 0, 2, 1, 5, 1, 0, 1, 1, 10, 1, 0, 0)),
      (Registers(3, 15, 1, 0), Vector(null, 4, 0, 0, 0, 2, 1, 5, 1, 0, 1, 1, 10, 1, 0, 0)),
      // leave: foo(0)
      (Registers(2, 10, 1, 0), Vector(null, 4, 0, 0, 0, 2, 1, 5, 1, 0, 0)),
      (Registers(0, 10, 1, 0), Vector(null, 4, 0, 0, 0, 2, 1, 5, 1, 0, 0)),
      (Registers(3, 10, 1, 0), Vector(null, 4, 0, 0, 0, 2, 1, 5, 1, 0, 0)),
      // leave: foo(1)
      (Registers(2, 5, 1, 0), Vector(null, 4, 0, 0, 0, 0)),
      (Registers(0, 5, 1, 0), Vector(null, 4, 0, 0, 0, 0)),
      (Registers(3, 5, 1, 0), Vector(null, 4, 0, 0, 0, 0)),
      // leave: foo(2)
      (Registers(5, 0, 0, 0), Vector(0))
    )
  }

  "The SafeFunRuntime" should "detected accesses to protected heap locations" in {
    val prog = program(
      List(
        // newpw is entered by the user
        NewArrayStmt("newpw", Lit(Num(8))),
        AssignArrayStmt("newpw", Lit(Num(0)), Lit(Num(18))),
        AssignArrayStmt("newpw", Lit(Num(1)), Lit(Num(0))),
        AssignArrayStmt("newpw", Lit(Num(2)), Lit(Num(0))),
        AssignArrayStmt("newpw", Lit(Num(3)), Lit(Num(0))),
        // set userid in `grantAdminAccess` to 14
        AssignArrayStmt("newpw", Lit(Num(4)), Lit(Num(14))),
        // set the return address to `grantAdminAccess`
        AssignArrayStmt("newpw", Lit(Num(6)), Lit(Num(6))),
        // set the frame of `grantAdminAccess` to the beginning of the user controlled password
        AssignArrayStmt("newpw", Lit(Num(7)), Lit(Num(8))),
        NewArrayStmt("pw", Lit(Num(6))),
        CallStmt("ret", "setpassword", List(Lit(Num(14)), Var("pw"), Var("newpw"))),
        CallStmt("ret", "grantAdminAccess", List(Lit(Num(1))))
      ),
      Function("setpassword", List("userid", "pw", "newpw"), List(
        AssignStmt("i", Sub(ArrayLength("newpw"), Lit(Num(1)))),
        WhileStmt(Or(Gt(Var("i"), Lit(Num(0))), Eq(Var("i"), Lit(Num(0)))), List(
          ReadArrayStmt("x", "newpw", Var("i")),
          AssignArrayStmt("pw", Var("i"), Var("x")),
          AssignStmt("i", Sub(Var("i"), Lit(Num(1))))
        )),
        ReturnStmt(Var("pw"))
      )),
      Function("grantAdminAccess", List("userid"), List(
        // Grant the user with the given id admin access.
        ReturnStmt(Var("userid"))
      ))
    )

    val instrs = List(
      // setpassword
      AssignInstr(3,Sub(ArrayLength(2),Lit(Num(1)))),
      JumpIfInstr(6,Not(Or(Gt(Var(3),Lit(Num(0))),Eq(Var(3),Lit(Num(0)))))),
      ReadArrayInstr(4,2,Var(3)),
      AssignArrayInstr(1,Var(3),Var(4)),
      AssignInstr(3,Sub(Var(3),Lit(Num(1)))),
      JumpIfInstr(1,Lit(Bool(true))),
      ReturnInstr(Var(1)),
      // grantAdminAccess
      ReturnInstr(Var(0)),
      // main
      NewArrayInstr(0,Lit(Num(8))),
      AssignArrayInstr(0, Lit(Num(0)), Lit(Num(18))),
      AssignArrayInstr(0, Lit(Num(1)), Lit(Num(0))),
      AssignArrayInstr(0, Lit(Num(2)), Lit(Num(0))),
      AssignArrayInstr(0, Lit(Num(3)), Lit(Num(0))),
      AssignArrayInstr(0, Lit(Num(4)), Lit(Num(14))),
      AssignArrayInstr(0, Lit(Num(6)), Lit(Num(6))),
      AssignArrayInstr(0, Lit(Num(7)), Lit(Num(8))),
      NewArrayInstr(1,Lit(Num(6))),
      CallInstr(2,0,5,List(Lit(Num(14)), Var(1), Var(0))),
      CallInstr(2,7,1,List(Lit(Num(1))))
    )

    FunCompiler.compile(prog)._1 shouldBe instrs

    an [RuntimeException] should be thrownBy SafeFunRuntime.run(prog)

  }

  def theTrace(instr: List[Instruction], l: (Registers, Vector[Value])*): List[State] = {
    l.map { case (regs, mem) => State(instr, regs, Heap(mem,List())) }.toList
  }

  def equivalentTo(instr: List[Instruction], l: (Registers, Map[Loc,Value])* ): List[State] = {
    l.map {
      case (regs, mem) => {
        val newMem = Heap.empty.alloc(mem.size)._1
        State(instr, regs, mem.foldLeft(newMem) { case (m, (l, v)) => m.store(l, v)})
      }
    }.toList
  }

  implicit def int2numvalue(x: Int): Value = NumValue(x)
}
