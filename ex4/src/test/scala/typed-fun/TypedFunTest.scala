package typedfun

import org.scalatest._
import common.Lang._
import common.Runtime._
import typedfun.FunLang._
import typedfun.FunInstrs._
import typedfun.FunCompiler._
import typedfun.FunRuntime._
import typedfun.FunTypeChecker._

import scala.util
import scala.util.{Failure, Success, Try}

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

  "An assign statement" should "type check" in {
    val prog = program(
      List(
        AssignStmt("x", Lit(Num(2))),
        AssignStmt("y", Lit(Bool(true))),
        AssignStmt("z", Var("x"))
      )
    )

    typeCheckProgram(prog) shouldBe Success()
  }

  "An assign statement" should "not type check" in {
    val prog = program(
      List(
        AssignStmt("x", Lit(Num(2))),
        AssignStmt("y", Lit(Bool(true))),
        AssignStmt("z", Var("a")) //bad
      )
    )
    typeCheckProgram(prog) shouldBe a[Failure[TypeError]]
  }

  "A while statement" should "type check" in {
    val prog = program(
      List(
        WhileStmt(Lit(Bool(true)), List())
      )
    )

    typeCheckProgram(prog) shouldBe Success()
  }

  "A while statement" should "not type check" in {
    val prog = program(
      List(
        WhileStmt(Lit(Num(1)), List())
      )
    )
    typeCheckProgram(prog) shouldBe a[Failure[TypeError]]
  }

  "An if statement" should "type check" in {
    val prog = program(
      List(
        IfStmt(Lit(Bool(true)), List(), List())
      )
    )

    typeCheckProgram(prog) shouldBe Success()
  }

  "An if statement" should "not type check on ill expression" in {
    val prog = program(
      List(
        IfStmt(Lit(Num(1)), List(), List())
      )
    )
    typeCheckProgram(prog) shouldBe a[Failure[TypeError]]
  }

  "An if statement" should "not type check on ill branch" in {
    val prog = program(
      List(
        IfStmt(Lit(Bool(true)), List(AssignStmt("z", Var("a"))), List())
      )
    )
    typeCheckProgram(prog) shouldBe a[Failure[TypeError]]
  }

  "An AssignArray statement" should "type check" in {
    val prog = program(
      List(
        AssignArrayStmt("x", Lit(Num(1)), Lit(Num(1))),
        AssignArrayStmt("y", Lit(Num(1)), Var("x"))
      )
    )

    typeCheckProgram(prog) shouldBe Success()
  }

  "An AssignArray statement" should "not type check on ill index" in {
    val prog = program(
      List(
        AssignArrayStmt("x", Lit(Bool(true)), Lit(Num(1)))
      )
    )
    typeCheckProgram(prog) shouldBe a[Failure[TypeError]]
  }

  "An AssignArray statement" should "not type check on ill value" in {
    val prog = program(
      List(
        AssignArrayStmt("x", Lit(Bool(true)), Var("e"))
      )
    )
    typeCheckProgram(prog) shouldBe a[Failure[TypeError]]
  }

  "A ReadArray statement" should "type check" in {
    val prog = program(
      List(
        NewArrayStmt("x", NumberType(), Lit(Num(1))),
        ReadArrayStmt("y", "x", Lit(Num(1)))
      )
    )

    typeCheckProgram(prog) shouldBe Success()
  }

  "A ReadArray statement" should "not type check on non existing array" in {
    val prog = program(
      List(
        ReadArrayStmt("x", "y", Lit(Num(1)))
      )
    )
    typeCheckProgram(prog) shouldBe a[Failure[TypeError]]
  }

  "A ReadArray statement" should "not type check on ill index" in {
    val prog = program(
      List(
        ReadArrayStmt("x", "y", Lit(Bool(true)))
      )
    )
    typeCheckProgram(prog) shouldBe a[Failure[TypeError]]
  }

  "A Call statement" should "type check" in {
    val prog = program(
      List(
        CallStmt("x", "foo", List())
      ),
      Function(NumberType(), "foo", List(), List())
    )

    typeCheckProgram(prog) shouldBe Success()
  }

  "A Call statement" should "not type check on ill arguments" in {
    val prog = program(
      List(
        CallStmt("x", "foo", List(Lit(Bool(true)), Lit(Bool(true))))
      ),
      Function(NumberType(), "foo", List(Argument("a",NumberType()), Argument("b",NumberType())), List())
    )
    typeCheckProgram(prog) shouldBe a[Failure[TypeError]]
  }

  "A Call statement" should "not type check on non existing function" in {
    val prog = program(
      List(
        CallStmt("x", "foo", List())
      ),
      Function(NumberType(), "bar", List(), List())
    )
    typeCheckProgram(prog) shouldBe a[Failure[TypeError]]
  }

  "A Return statement" should "type check" in {
    val prog = program(
      List(),
      Function(NumberType(), "foo", List(), List(
        ReturnStmt(Lit(Num(1)))
      )),
      Function(BooleanType(), "foo", List(), List(
        ReturnStmt(Lit(Bool(true)))
      ))
    )

    typeCheckProgram(prog) shouldBe Success()
  }

  //TODO: how would that be possible to check? -> see FunTypeChecker:92
  /*
  "A Return statement" should "not type check on ill return type" in {
  val prog = program(
    List(),
    Function(NumberType(), "foo", List(), List(
      ReturnStmt(Lit(Bool(true)))
    )),
    Function(BooleanType(), "foo", List(), List(
      ReturnStmt(Lit(Num(1)))
    ))
    )
    typeCheckProgram(prog) shouldBe a[Failure[TypeError]]
  }*/
}
