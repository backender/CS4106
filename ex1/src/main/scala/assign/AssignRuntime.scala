import common.Lang._
import common.Runtime._

import AssignLang._
import AssignInstrs._
import AssignCompiler._

object AssignRuntime {

  case class State(program: List[Instruction], programCounter: Int, heap: Heap) {
    override def toString() = {
      "(%s, %s)".format(programCounter, heap)
    }
  }

  type Trace = List[State]

  def trace(state: State) : Trace = {
    try {
      if (state.programCounter < state.program.length)
        state :: trace(step(state))
      else
        state :: List()
    } catch {
      case e: Exception =>
        e.printStackTrace()
        state :: List()
    }
  }

  def run(prog: Program): Trace = {
    val (instrs, initHeapSize) = compile(prog)
    val (heap, _) = Heap.empty.alloc(initHeapSize)
    trace(State(instrs, 0, heap))
  }

  def step(state: State) : State = {
    var heap = state.heap
    var pc = state.programCounter
    var ifElseInstr : List[Instruction] = List()

    state.program(state.programCounter) match {
      case AssignInstr(res, expr) => {
        heap = heap.store(res, evalExpr(heap, expr))
        pc += 1
      }

      // TODO: Handle the case of ifjump's
      case IfStmtInstr(expr, ifInstrs, elseInstrs) => {
        pc += 1
        if (ensureBool(evalExpr(heap, expr))) {
          ifElseInstr = ifInstrs
        } else {
          ifElseInstr = elseInstrs
        }
      }

      case WhileStmtInstr(expr, branchInstr) => {
        if (ensureBool(evalExpr(heap, expr))) {
          ifElseInstr = branchInstr
        } else {
          pc += 1
        }
      }

    }

    state.copy(
      program = state.program ++ ifElseInstr,
      programCounter = pc,
      heap = heap
    )
  }

  def evalExpr(heap: Heap, expr: Expression[Loc]): Value = expr match {
    case Var(idx) => heap.fetch(idx).getOrElse(sys.error("Variable at index %d has not been set yet".format(idx)))

    case Lit(Num(x)) => NumValue(x)
    case Lit(Bool(x)) => BoolValue(x)

    case Neg(e) => NumValue(-ensureNum(evalExpr(heap, e)))
    case Add(e1, e2) => NumValue(ensureNum(evalExpr(heap, e1)) + ensureNum(evalExpr(heap, e2)))
    case Sub(e1, e2) => NumValue(ensureNum(evalExpr(heap, e1)) - ensureNum(evalExpr(heap, e2)))
    case Mul(e1, e2) => NumValue(ensureNum(evalExpr(heap, e1)) * ensureNum(evalExpr(heap, e2)))
    case Div(e1, e2) => NumValue(ensureNum(evalExpr(heap, e1)) / ensureNum(evalExpr(heap, e2)))

    case Not(e) => BoolValue(!ensureBool(evalExpr(heap, e)))
    case And(e1, e2) => BoolValue(ensureBool(evalExpr(heap, e1)) && ensureBool(evalExpr(heap, e2)))
    case Or(e1, e2) => BoolValue(ensureBool(evalExpr(heap, e1)) || ensureBool(evalExpr(heap, e2)))
    case Eq(e1, e2) => BoolValue(evalExpr(heap, e1) == evalExpr(heap, e2))
  }

  def ensureNum(v: Value): Int = v match {
    case NumValue(x) => x
    case _ => sys.error("expected number, but got %s".format(v))
  }

  def ensureBool(v: Value): Boolean = v match {
    case BoolValue(x) => x
    case _ => sys.error("expected boolean, but got %s".format(v))
  }

  def printTrace(trace: Trace) = {
    for (state <- trace) {
      if (state.programCounter < state.program.length) {
        printf("Instruction: %s\n", state.program(state.programCounter))
      } else {
        printf("Halted\n")
      }
      printf("Heap: %s\n\n", state.heap.memory)
    }
  }
}
