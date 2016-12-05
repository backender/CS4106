import common.Lang._
import common.Runtime._

import FunLang._
import FunInstrs._
import FunCompiler._

object SafeFunRuntime {

  type Reg = Int

  case class Registers(programCounter: Reg, framePointer: Reg, frameSize: Reg, returnValue: Reg) {
    def count : Int = productArity
    def asList : List[Reg] = List(programCounter, framePointer, frameSize, returnValue)
  }
  object Registers {
    def fromList(rs: List[Reg]) = Registers(rs(0), rs(1), rs(2), rs(3))
  }

  case class State(program: List[Instruction], registers: Registers, heap: Heap, protect: Set[Loc]) {
    override def toString() = "(%s, %s)".format(registers, heap)
  }


  def program(main: List[Statement], funcs: Function*) = Program(funcs.toList, main)

  type Trace = List[State]

  def trace(state: State) : Trace = {
    if (state.registers.programCounter < state.program.length)
      state :: trace(step(state))
    else
      state :: List()
  }

  def run(prog: Program): Trace = {
    val (instrs, entryPoint, initFrameSize) = compile(prog)
    val (heap, idx) = Heap.empty.alloc(initFrameSize)
    trace(State(instrs, Registers(entryPoint, idx, 0, 0), heap, Set()))
  }

  def printTrace(trace: Trace) = {
    for (state <- trace) {
      if (state.registers.programCounter < state.program.length) {
        printf("Instruction: %s\n", state.program(state.registers.programCounter))
      } else {
        printf("Halted\n")
      }
      printf("Heap: %s\n\n", state.heap.memory)
    }
  }


  def step(state: State) : State = {
    var regs = state.registers
    var heap = state.heap
    var protect = state.protect

    state.program(regs.programCounter) match {
      case AssignInstr(res, expr) => {
        regs = regs.copy(programCounter = regs.programCounter + 1)
        heap = heap.store(regs.framePointer + res, evalExpr(heap, regs, expr))
      }

      case JumpIfInstr(loc, condExpr) =>
        if (ensureBool(evalExpr(heap, regs, condExpr))) {
          regs = regs.copy(programCounter = loc)
        } else {
          regs = regs.copy(programCounter = regs.programCounter + 1)
        }

      case AssignArrayInstr(x, i, e) => {
        val arrayStart = ensureNum(heap.fetch(regs.framePointer + x)
          .getOrElse(sys.error("Array at index %d has not been set yet".format(x))))
        val idx = ensureNum(evalExpr(heap, regs, i)) + 1
        regs = regs.copy(programCounter = regs.programCounter + 1)
        heap = heap.store(arrayStart + idx, evalExpr(heap, regs, e))
      }

      case ReadArrayInstr(x, a, i) => {
        val arrayStart = ensureNum(heap.fetch(regs.framePointer + a)
          .getOrElse(sys.error("Array at index %d has not been set yet".format(a))))
        val idx = ensureNum(evalExpr(heap, regs, i)) + 1
        regs = regs.copy(programCounter = regs.programCounter + 1)
        heap = heap.store(regs.framePointer + x, heap.fetch(arrayStart + idx)
          .getOrElse(sys.error("Array index %d of array %d has not been set".format(idx, a))))
      }

      case NewArrayInstr(x, len) => {
        val size = ensureNum(evalExpr(heap, regs, len))
        val (newHeap, loc) = heap.alloc(size + 1)

        regs = regs.copy(programCounter = regs.programCounter + 1)
        heap = newHeap.store(regs.framePointer + x, NumValue(loc))
          .store(regs.framePointer + loc, NumValue(size))
      }

      case CallInstr(ret, pos, frameSize, args) => {
        // Allocate frame on heap
        var (newHeap, loc) = heap.alloc(frameSize + regs.count)

        // Save current registers
        for ((reg, i) <- regs.asList.zipWithIndex) {
          newHeap = newHeap.store(loc + i, NumValue(reg))
        }

        // Store arguments
        val newFramePointer = loc + regs.count
        for ((arg, i) <- args.zipWithIndex) {
          newHeap = newHeap.store(newFramePointer + i, evalExpr(heap, regs, arg))
        }

        regs = regs.copy(
          programCounter = pos,
          framePointer = newFramePointer,
          returnValue = ret,
          frameSize = frameSize
        )
        heap = newHeap
      }

      case ReturnInstr(expr) => {
        // evaluate expressions
        val v = evalExpr(heap, regs, expr)

        // restore old registers
        val oldRegsData = heap.chunk(regs.framePointer - regs.count, regs.count)
        val oldRegs = Registers.fromList(oldRegsData.map(ensureNum(_)))

        // store function return value in old stack frame
        var newHeap: Heap = heap.store(oldRegs.framePointer + regs.returnValue, v)

        // free up current frame on heap
        val from = regs.framePointer - regs.count
        val to = regs.framePointer + regs.frameSize - 1
        newHeap = newHeap.free(from, to)

        heap = newHeap
        regs = oldRegs.copy(programCounter = oldRegs.programCounter + 1)
      }
    }

    state.copy(
      registers = regs,
      heap = heap,
      protect = protect
    )
  }

  def evalExpr(heap: Heap, regs: Registers, expr: Expression[Loc]): Value = expr match {
    case Var(idx) =>
      heap.fetch(regs.framePointer + idx).getOrElse(sys.error("Variable at index %d has not been set yet".format(idx)))

    case ArrayLength(idx) => {
      val arrayStart = ensureNum(heap.fetch(regs.framePointer + idx)
        .getOrElse(sys.error("Array at index %d has not been set yet".format(idx))))
      heap.fetch(arrayStart).getOrElse(sys.error("Array at index %d has not been set yet".format(idx)))
    }

    case Lit(Num(x)) => NumValue(x)
    case Lit(Bool(x)) => BoolValue(x)

    case Neg(e) => NumValue(-ensureNum(evalExpr(heap, regs, e)))
    case Add(e1, e2) => NumValue(ensureNum(evalExpr(heap, regs, e1)) + ensureNum(evalExpr(heap, regs, e2)))
    case Sub(e1, e2) => NumValue(ensureNum(evalExpr(heap, regs, e1)) - ensureNum(evalExpr(heap, regs, e2)))
    case Mul(e1, e2) => NumValue(ensureNum(evalExpr(heap, regs, e1)) * ensureNum(evalExpr(heap, regs, e2)))
    case Div(e1, e2) => NumValue(ensureNum(evalExpr(heap, regs, e1)) / ensureNum(evalExpr(heap, regs, e2)))

    case Not(e) => BoolValue(!ensureBool(evalExpr(heap, regs, e)))
    case And(e1, e2) => BoolValue(ensureBool(evalExpr(heap, regs, e1)) && ensureBool(evalExpr(heap, regs, e2)))
    case Or(e1, e2) => BoolValue(ensureBool(evalExpr(heap, regs, e1)) || ensureBool(evalExpr(heap, regs, e2)))
    case Eq(e1, e2) => BoolValue(evalExpr(heap, regs, e1) == evalExpr(heap, regs, e2))
    case Lt(e1, e2) => BoolValue(ensureNum(evalExpr(heap, regs, e1)) < ensureNum(evalExpr(heap, regs, e2)))
    case Gt(e1, e2) => BoolValue(ensureNum(evalExpr(heap, regs, e1)) > ensureNum(evalExpr(heap, regs, e2)))
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
