package array

import common.Lang._
import common.Runtime._

import array.ArrayLang._
import array.ArrayInstrs._

object SafeArrayRuntime {

  import array.ArrayRuntime.{evalExpr, ensureNum, ensureBool}

  case class State(program: List[Instruction], programCounter: Int, heap: Heap, deleted: Set[Loc]) {
    override def toString() = {
      "(%s, %s, %s)".format(programCounter, heap, deleted)
    }

    override def equals(that: Any) : Boolean =
      that match {
        case other: State =>
          this.program.equals(other.program) && this.programCounter.equals(other.programCounter) && this.heap.equals(other.heap)
        case _ => false
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
    trace(State(instrs, 0, heap, Set()))
  }

  // TODO: modify the step function. Make use of the `deleted` field
  // in `State` to keep track of memory locations that have been freed.
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

}