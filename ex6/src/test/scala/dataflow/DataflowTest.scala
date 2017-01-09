package dataflow

import array.ArrayLang._
import common.Lang._
import dataflow.ControlFlowGraph._
import org.scalatest._


class DataflowTest extends FlatSpec with Matchers {

  "Instructions without control flow" should "produce a linear CFG" in {
    val prog =
      List(
        AssignStmt("x", Lit(Num(1))),
        AssignStmt("y", Lit(Num(2))),
        AssignStmt("z", Lit(Num(3)))
      )

    controlFlowGraph(prog) shouldBe
      CFG(
        prog,
        Map(
          0 -> List(1),
          1 -> List(2)),
        Map(
          2 -> List(1),
          1 -> List(0)))
  }

  "Instructions with simple if condition" should "produce a CFG" in {
    val prog =
      List(
        IfStmt(
          Lit(Num(1)),
          List(AssignStmt("y", Lit(Num(2)))),
          List(AssignStmt("z", Lit(Num(3)))))
      )

    controlFlowGraph(prog) shouldBe
      CFG(
        prog,
        Map(
          0 -> List(1, 2)
        ),
        Map(
          2 -> List(0),
          1 -> List(0)))
  }
/*
  "Binary search" should "the correct CFG" in {
    val prog = List(
      ArrayInitStmt("arr", List(Lit(Num(1)),Lit(Num(2)),Lit(Num(3)),Lit(Num(4)),Lit(Num(5)))),
      AssignStmt("search", Lit(Num(4))),
      AssignStmt("first", Lit(Num(0))),
      AssignStmt("last", Lit(Num(4))),
      AssignStmt("found", Lit(Num(-1))),
      AssignStmt("middle", Div(Add(Var("first"),Var("last")),Lit(Num(2)))),
      WhileStmt(And(Or(Lt(Var("first"),Var("last")), Eq(Var("first"), Var("last"))), Eq(Var("found"), Lit(Num(-1)))), List(
        ReadArrayStmt("x", "arr", Var("middle")),
        IfStmt(Lt(Var("x"), Var("search")), List(
            AssignStmt("first", Add(Var("middle"), Lit(Num(1))))
          ),
          List(
            IfStmt(Eq(Var("x"), Var("search")), List(
                AssignStmt("found", Var("middle"))
              ),
              List(
                AssignStmt("last", Sub(Var("middle"), Lit(Num(-1)))))
              )
            )
          ),
        AssignStmt("middle", Div(Add(Var("first"),Var("last")),Lit(Num(2))))
      ))
    )
//println(controlFlowGraph(prog))
    controlFlowGraph(prog) shouldBe ???
  }*/
}
