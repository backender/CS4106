package common

import Lang._

object Runtime {
  // Variable location in instruction
  type Loc = Int
  // Instruction position
  type Pos = Int
  // A free block of memory
  type Block = (Loc,Loc)

  sealed case class Heap(memory: Vector[Value], freeBlocks: List[Block]) {
    def store(loc: Loc, value: Value) : Heap = Heap(memory.updated(loc, value), freeBlocks)
    def fetch(loc: Loc) : Option[Value] = memory.lift(loc)
    def chunk(loc: Loc, size: Int) : List[Value] = memory.slice(loc, loc + size).toList

    def size() : Int = memory.size

    def alloc(size: Int) : (Heap, Loc) = {
      // Look for a suitable free block
      val i = freeBlocks.indexWhere{ case (from,to) => to-from+1 >= size }

      if (i == -1) {
        // If no suitable free block could be found, add a new chunk to the end of the memory.
        val chunk = List.range(memory.length, memory.length + size).map(l => null)
        (Heap(memory ++ chunk,freeBlocks), this.size)
      } else {
        // If a sutable free block could be found, split the free block into an allocated piece
        // and a residual block.
        val (from,to) = freeBlocks(i)
        val fb =
          if(to-from+1 == size)
            freeBlocks.patch(i,List(),1)
          else
            freeBlocks.patch(i,List((from+size,to)),1)
        (Heap(memory,fb),from)
      }
    }

    // Free does not actually delete the memory, it only marks the region as free.
    def free(from: Loc, to: Loc) : Heap = {
      Heap(memory, (from,to) :: freeBlocks)
    }

    // Equals only compares the memory, not the list of free blocks
    override def equals(that: Any) : Boolean =
      that match {
        case other: Heap => this.usedMemoryMap.equals(other.usedMemoryMap)
        case _ => false
      }

    private def usedMemoryMap: Map[Int, Value] = {
      var free = Set[Int]()
      for ((from, to) <- freeBlocks;
           i <- from.to(to))
        free += i

      var used = Map[Int, Value]()
      for ((v, i) <- memory.zipWithIndex if !free.contains(i))
        used += (i -> v)

      used
    }

    private def usedMemoryVector: Vector[Value] = {
      var free = Set[Int]()
      for ((from, to) <- freeBlocks;
           i <- from.to(to))
        free += i

      memory.zipWithIndex.map{case (v,i) => if (free.contains(i)) null else v}
    }

    override def toString = s"Heap(${usedMemoryVector.toString})"
  }

  object Heap {
    def empty() : Heap = Heap(Vector.empty, List())
  }
}
