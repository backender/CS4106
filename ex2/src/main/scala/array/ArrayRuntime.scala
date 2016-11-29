package array

import common.Lang._
import common.Runtime._

import array.ArrayLang._
import array.ArrayInstrs._

object ArrayRuntime {

  case class State(program: List[Instruction], programCounter: Int, heap: Heap) {
    override def toString() = {
      "(%s, %s)".format(programCounter, heap)
    }
  }

  def program(statements: Statement*) = Program(statements.toList)

  type Trace = List[State]

  def trace(state: State) : Trace =
    if (state.programCounter < state.program.length)
      state :: trace(step(state))
    else
      state :: List()

  def run(compiler: ArrayCompiler, prog: Program): Trace = {
    val (instrs, initHeapSize) = compiler.compile(prog)
    val (heap, _) = Heap.empty.alloc(initHeapSize)
    trace(State(instrs, 0, heap))
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


  def step(state: State) : State = {
    var pc = state.programCounter
    var heap = state.heap

    state.program(pc) match {
      case AssignInstr(res, expr) => {
        pc += 1
        heap = heap.store(res, evalExpr(heap, expr))
      }

      case JumpIfInstr(loc, condExpr) =>
        if (ensureBool(evalExpr(heap, condExpr))) {
          pc = loc
        } else {
          pc += 1
        }

      case AssignArrayInstr(x, i, e) => {
        val arrayStart = ensureNum(heap.fetch(x).getOrElse(sys.error("Array at index %d has not been set yet".format(x))))
        val idx = ensureNum(evalExpr(heap, i)) + 1
        pc += 1
        heap = heap.store(arrayStart + idx, evalExpr(heap, e))
      }

      case ReadArrayInstr(x, a, i) => {
        val arrayStart = ensureNum(heap.fetch(a).getOrElse(sys.error("Array at index %d has not been set yet".format(a))))
        val idx = ensureNum(evalExpr(heap, i)) + 1
        pc += 1
        heap = heap.store(x, heap.fetch(arrayStart + idx)
          .getOrElse(sys.error("Array index %d of array %d has not been set".format(idx, a))))
      }

      case NewArrayInstr(x, len) => {
        val size = ensureNum(evalExpr(heap, len))
        val (newHeap, pos) = heap.alloc(size + 1)
        pc += 1
        heap = newHeap.store(x, NumValue(pos))
          .store(pos, NumValue(size))
      }

      case DeleteArrayInstr(a) => {
        val arrayStart = ensureNum(heap.fetch(a)
          .getOrElse(sys.error("Array at index %d has not been set yet".format(a))))
        val arrayLen = ensureNum(heap.fetch(arrayStart + 0)
          .getOrElse(sys.error("Array index %d of array %d has not been set".format(0, a))))
        pc += 1
        heap = heap.free(arrayStart, arrayStart + arrayLen)
      }

      case AbortInstr(msg) =>
        throw new RuntimeException(msg)
    }

    state.copy(
      programCounter = pc,
      heap = heap
    )
  }

  def evalExpr(heap: Heap, expr: Expression[Loc]): Value = expr match {
    case Var(idx) =>
      heap.fetch(idx).getOrElse(sys.error("Variable at index %d has not been set yet".format(idx)))

    case ArrayLength(idx) => {
      val arrayStart = ensureNum(heap.fetch(idx).getOrElse(sys.error("Array at index %d has not been set yet".format(idx))))
      heap.fetch(arrayStart).getOrElse(sys.error("Array at index %d has not been set yet".format(idx)))
    }

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
    case Lt(e1, e2) => BoolValue(ensureNum(evalExpr(heap, e1)) < ensureNum(evalExpr(heap, e2)))
    case Gt(e1, e2) => BoolValue(ensureNum(evalExpr(heap, e1)) > ensureNum(evalExpr(heap, e2)))
  }

  def ensureNum(v: Value): Int = v match {
    case NumValue(x) => x
    case _ => sys.error("expected number, but got %s".format(v))
  }

  def ensureBool(v: Value): Boolean = v match {
    case BoolValue(x) => x
    case _ => sys.error("expected boolean, but got %s".format(v))
  }

}
