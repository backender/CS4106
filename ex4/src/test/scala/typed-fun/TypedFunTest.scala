package typedfun

import org.scalatest._
import common.Lang._
import common.Runtime._
import typedfun.FunLang._
import typedfun.FunInstrs._
import typedfun.FunCompiler._
import typedfun.FunRuntime._
import typedfun.FunTypeChecker._

import scala.util.{Try, Success, Failure}

class TypedFunTest extends FlatSpec with Matchers {

  "The empty program" should "type check" in {
    val prog = program(List())
    typeCheckProgram(prog) shouldBe Success()
  }

  "The main function" should "type check" in {
    val prog = program(
      List(
        AssignStmt("x", Lit(Num(42)))
      )
    )

    typeCheckProgram(prog) shouldBe Success()
  }

  "A program with one function" should "type check" in {
    val prog = program(
      List(
        CallStmt("x", "foo", List(Lit(Num(2)), Lit(Num(6))))
      ),
      Function(NumberType(), "foo", List(Argument("a",NumberType()), Argument("b",NumberType())), List(
        AssignStmt("c", Mul(Var("a"), Var("b"))),
        ReturnStmt(Var("c"))
      ))
    )
    typeCheckProgram(prog) shouldBe Success()
  }

  "A recursive function" should "type check" in {
    val prog = program(
      List(
        CallStmt("x", "foo", List(Lit(Num(2))))
      ),
      Function(NumberType(), "foo", List(Argument("a",NumberType())), List(
        WhileStmt(Gt(Var("a"), Lit(Num(0))), List(
          CallStmt("a", "foo", List(Sub(Var("a"), Lit(Num(1)))))
        )),
        ReturnStmt(Var("a"))
      ))
    )

    typeCheckProgram(prog) shouldBe Success()
  }
}
