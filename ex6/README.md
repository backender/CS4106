# Substructural Type Systems

Context of this exercise is the array language.

- Task 1: Draw a control flow graph for the following program: (1pt)

     0: arr := { 1, 2, 3, 4, 5 };

     1: search := 4;
     2: first := 0;
     3: last := 4;
     4: found := -1;
     5: middle := (first+last) / 2;

     6: while(first <= last && found == -1) {
     7:   x := arr[middle]
     8:   if(x < search) {
     9:     first := middle + 1;
          } else {
    10:     if(x == search) {
    11:       found := middle
            } else {
    12:       last := middle - 1;
            }
          }

    13:   middle := (first + last) / 2;
        }
    14: res := found

- Task 2: In the provided code base, a control flow graph consists of a flattened list of statements, an adjacency list of predecessors and successors. Implement the function `controlFlowGraph(List[Statement], CFG, Int)` that builds up a control flow graph from a list of statements. Write two tests to validate that your implementation is correct. (2pt)

- Task 3: Execute 3 iterations of the analysis introduced in the lecture for the program of task 1 for code blocks 6 and 14. "3 iterations" means: (2pt)

    for N in {6,14}
    AR_entry0(N) = {}
    AR_exit0(N) = {}

    AR_entry1(N) := Join {AR_exit0(M) | M -> N}
    AR_exit1(N) := AR where (AR_entry0(N) |- N ~ AR)

    AR_entry2(N) := Join {AR_exit1(M) | M -> N}
    AR_exit2(N) := AR where (AR_entry1(N) |- N ~ AR)

    AR_entry3(N) := Join {AR_exit2(M) | M -> N}
    AR_exit3(N) := AR where (AR_entry2(N) |- N ~ AR)

Has a fixed-point be reached after 3 iterations, i.e. for all s, `AR_entry4(s) = AR_entry3(s)` and `AR_exit4(s) = AR_exit3(s)`? If not, explain if a fixed-point will be reached in more iterations or why a fixed-point will never be reached for this program.

- Task 4: We now change the lattice of our analysis to minimal and maximal interval bounds of -5 and 5. If an arithmetic operation exceeds these limits, the interval bound is set to -∞ or ∞ respectively. Repeat the analysis of task 3, with the new lattice until a fixed-point is reached. (2pt)

- Task 5: Discuss: Does the analysis always reach a fixed-point for all programs? What is the trade-off between the analyses in task 3 and 4. (1pt)

- Task 6: Implement the analysis of task 4 by implementing the `???` in `BoundedDataflowAnalysis.scala`. Write to tests to validate that your implementation is correct. (2pt)