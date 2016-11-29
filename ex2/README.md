# Dynamic analysis of the Array language

In this exercises we develop dynamic analyses for the Array language,
by modifying the compiler and by monitoring the traces of a running
program. The goal of the analysis is to detect unsafe memory access
that can happen with unmanaged memory. For this exercises we extend
the Array language of the lecture with statements that free up memory
of arrays.

    Prog ::= Stmt*
    Stmt ::= Identifier := Expr | while Expr Stmt*
      | Identifier := arnew Expr 
      | Identifier := arread Identifier Expr
      | arwrite Identifier Expr Expr 
      | delete Identifier
    Expr ::= Identifier | Literal | Expr {+,-,/,*,&&,||,=} Expr | {-,!} Expr
      | arlen Identifier
    Literal ::= Number | true | false
    Identifier ::= String

The semantics of `delete` is to retrieve the start of the array and to free the
dynamically allocated region of memory.

    P[pc] = delete a
    // Retrieve the start of the array
    get(m, a) = start
    // Retrieve the size of the array
    get(m, start) = size
    // Free the region of memory containing the array
    free(m, start, start + size) = m'
    -----------------------
    P, pc, m -> P, pc+1, m'

The unmanaged memory model has pitfalls, causing unexpected and
unpredictable behavior in our language. There are three kinds of
problems associated with the memory model:

 1. Memory leaks: a piece of dynamically allocated memory is not
    referred to by any variable.

 2. Accessing freed memory: Reading and writing to freed memory causes
    unpredictable effects.

 3. Double frees: Freeing twice can cause that the freed memory is
    allocated twice afterwards.

Task 1: Provide an example program in the Array language for each
problem.

We prepared again an implementation of the Array language that is
available in the attachments of this exercise.

Task 2: Study the concrete implementation of `alloc` and `free` in
`common.Runtime`. Add 3 tests to `ArrayTest` for the programs of Task
1 to confirm that they actually have these problems.

# Dynamic analysis by instrumenting the compiler

Next, we want to implement a dynamic analysis to circumvent the
problem of accessing freed memory and double frees by instrumenting
the compiler. We added an instruction to the abstract machine to abort
the execution of the program with a `RuntimeException`.

    Instructions ::= ... | abort String

Task 3: Modify the `compileStatement` method in `SafeArrayCompiler` to
prevent the problems of accessing freed memory and double frees. Use
`abort` to exit the execution unsafe programs.

Task 4: Add 2 tests for the same programs as in task 1 compiled with
the `SafeArrayCompiler` and run with `ArrayRuntime`. Ensure that the
execution of these programs actually aborts. Use the ScalaTest matcher
`should be thrownBy` to test if the execution of the program aborts as
expected with an exception.

Example: `an [RuntimeException] should be thrownBy ArrayRuntime.run(compiler,prog)`


# Dynamic analysis by monitoring execution traces

Another dynamic analysis method is run-time monitoring. In this part
of the exercise, we want to implement run-time monitoring to
circumvent the problem of accessing freed memory and double frees by
modifying the existing run-time system.

Task 5: Modify the `step` function in `SafeArrayRuntime` to prevent
the problem of accessing freed memory and double frees. Make use of
the set of deleted array locations to track if an array has been
deleted or is still valid.

Task 6: Add 2 tests for the same programs as in task 1 compiled with
the `ArrayCompiler` and run with `SafeArrayRuntime`.