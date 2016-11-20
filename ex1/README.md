IMPORTANT: Add your solution to the README file in the archive and upload the solutions as one archive to blackboard.

# Conditionals

In the first part of this exercise, we extend the Assign language with
conditional statements.

Syntax of the Assign language with conditionals

    Prog ::= Stmt*
    Stmt ::= Identifier = Expr | if Expr Stmt* Stmt*
    Expr ::= Identifier | Literal | Expr {+,-,/,*,&&,||,=} Expr | {-,!} Expr
    Literal ::= Number | true | false
    Identifier ::= String

Task 1: Define the SOS relations for expressions and programs that add
conditionals to the assign language. Provide traces for 3 different
programs.

# Compile conditionals

We compile conditionals to a machine that features only two
instructions: one for assignment as before and one for conditional
jumps.

    Instr ::= Idx = Expr | jumpif Number Expr

For `jumpif`, the first component describes the target program
counter, which must be a constant number. The second component
describes the condition under which the jump takes place. That is, a
jump only takes place if the condition evaluates to `true` and is
skipped otherwise.

We reuse machine states unchanged:

    State ::= (Prog, PC, Heap)
    PC ::= Number
    Heap ::= empty | Literal; Heap

Task 2: Define the semantics of the Assign language with conditionals
as a state transition relation. Provide traces for 3 different
programs.

# Implementation of conditionals

To gain a detailed understanding of the Assign language with
conditionals and to facilitate experimentation, we will implement the
compiler and the abstract machine of the language in Scala. We use
[SBT](http://www.scala-sbt.org/) for building the project and
[ScalaTest](http://www.scalatest.org/) for testing. To compile the
project, run `sbt compile` on the command-line. With `sbt test` you
can execute the tests.

Task 3: Study the code of the Assign language and add two tests for
programs with assignments to `AssignTest.scala`.

Task 4: Implement conditionals for the Assign language. We already
extended the expression type with conditionals. Please do not change
any of the signatures of expressions, because this makes it impossible
for us to test your code! To solve this task, add conditionals jumps
to the instructions, extend the compiler to handle conditionals and
extend the runtime of the abstract machine to handle conditional
jumps. Add at least 3 tests to `AssignTest.scala`, that test programs
containing conditionals.

# The While language

Next we want to add while loops to the language:

Syntax of the While language

    Prog ::= Stmt*
    Stmt ::= Identifier = Expr | if Expr Stmt* Stmt* | while Expr Stmt*
    Expr ::= Identifier | Literal | Expr {+,-,/,*,&&,||,=} Expr | {-,!} Expr
    Literal ::= Number | true | false
    Identifier ::= String

Task 5: Define the SOS relations for expressions and programs of the
While language. Provide traces for 3 different programs.

# Compiled While language

While loops are also compiled to conditional jumps. The set of
instructions and the abstract machine stay unchanged:

    State ::= (Prog, PC, Heap)
    PC ::= Number
    Heap ::= empty | Literal; Heap

Task 6: Define the semantics of the compiled While language as a state
transition relation. Provide traces for 3 different programs.

# Implementation of while loops

Task 7: Implement while loops for the Assign language with
conditionals. Use the same code base that you created for task 3. Add
at least 3 tests to `WhileTest.scala`, that test programs containing
while loops.

# Security vulnerabilities?

Task 8: Can you spot weaknesses in our model of While that may yield
security vulnerabilities later on? Describe any weakness you can find and
discuss whether it is exploitable or how it would become exploitable.