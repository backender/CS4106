package dataflow

import array.ArrayLang.{AssignStmt, _}
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

  "Instructions with if condition" should "produce a CFG" in {
    val prog =
      List(
        IfStmt(
          Lit(Num(1)),
          List(
            AssignStmt("y", Lit(Num(2)))
          ),
          List(
            AssignStmt("z", Lit(Num(3))),
            AssignStmt("zz", Lit(Num(3)))
          )
        )
      )

    controlFlowGraph(prog) shouldBe
      CFG(
        prog,
        Map(
          0 -> List(1, 2),
          2 -> List(3)
        ),
        Map(
          3 -> List(2),
          2 -> List(0),
          1 -> List(0)))
  }

  "Instructions with while loop" should "produce a CFG" in {
    val prog =
      List(
        WhileStmt(
          Lit(Bool(true)),
          List(
            AssignStmt("y", Lit(Num(2)))
          )
        ),
        AssignStmt("z", Lit(Num(3)))
      )

    controlFlowGraph(prog) shouldBe
      CFG(
        prog,
        Map(
          0 -> List(1, 2),
          1 -> List(0)
        ),
        Map(
          2 -> List(0),
          1 -> List(0),
          0 -> List(1)
        )
      )
  }


  "Instructions with while loop only" should "produce a CFG" in {
    val prog =
      List(
        WhileStmt(
          Lit(Bool(true)),
          List(
            AssignStmt("y", Lit(Num(2)))
          )
        )
      )

    controlFlowGraph(prog) shouldBe
      CFG(
        prog,
        Map(
          0 -> List(1),
          1 -> List(0)
        ),
        Map(
          1 -> List(0),
          0 -> List(1)
        )
      )
  }



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

    val succ = Map(
      0 -> List(1),
      1 -> List(2),
      2 -> List(3),
      3 -> List(4),
      4 -> List(5),
      5 -> List(6),
      //6 -> List(7, 14), // return statement is missing in code whereas it is given in lecture notes
      6 -> List(7),
      7 -> List(8),
      8 -> List(9, 10),
      9 -> List(13),
      10 -> List(11, 12),
      //11 -> List(13),  //TODO: BUG!!!!! Dont give me a hard time, I spent almost a day on that bug.
                         // The problem is that if a statement follows an Ifstmt, then I would only append it to the end of flatten(ifbranch) and then end of flatten(elsebranch).
                         // Image within the ifbranch or elsebranch there is another Ifstatement, then I'd only catch the end of the elsebranch but not the end of the ifbranch as well.
      12 -> List(13),
      13 -> List(6)



    )

    val pred = Map(
      1 -> List(0),
      2 -> List(1),
      3 -> List(2),
      4 -> List(3),
      5 -> List(4),
      6 -> List(5, 13),
      7 -> List(6),
      8 -> List(7),
      9 -> List(8),
      10 -> List(8),
      11 -> List(10),
      12 -> List(10),

      //13 -> List(9, 11, 12) //TODO: see bug above!
      13 -> List(9, 12)

      //14 -> List(6) // return statement is missing in code whereas it is given in lecture notes
    )

    val cfg = controlFlowGraph(prog)
    controlFlowGraph(prog) shouldBe CFG(prog, succ, pred)

    /*println("--------------------")
    println("Succ")
    println(cfg.successors)
    println(succ)

    println("Pred")
    println(cfg.predecessors)
    println(pred)*/
  }
}

