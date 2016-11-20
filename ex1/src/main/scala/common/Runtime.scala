package common

import Lang._

object Runtime {
  // Variable location in instruction
  type Loc = Int
  // Instruction position
  type Pos = Int

  sealed case class Heap(memory: Vector[Value]) {
    def store(loc: Loc, value: Value) : Heap = Heap(memory.updated(loc, value))
    def fetch(loc: Loc) : Option[Value] = memory.lift(loc)
    def chunk(loc: Loc, size: Int) : List[Value] = memory.slice(loc, loc + size).toList

    def size() : Int = memory.size

    def alloc(size: Int) : (Heap, Loc) = {
      // Allocate a new chunk of memory initialized with 0s
      val chunk = List.range(memory.length, memory.length + size).map(l => null)
      (Heap(memory ++ chunk), this.size)
    }

    def free(size: Int) : Heap = {
      // Remove elements from the end of the heap
      Heap(memory.slice(0, size))
    }
  }

  object Heap {
    def empty() : Heap = Heap(Vector.empty)
  }
}
