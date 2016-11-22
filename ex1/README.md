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

    Solution:
    
    expr -> expr'
    ---------------------------------------------
    if expr ss1 ss2 -> if expr' ss1 ss2
    
    expr -> true
    ---------------------------------------------
    if true ss1 ss2 -> ss1'
    
    expr -> false
    ---------------------------------------------
    if false ss1 ss2 -> ss2'


    Example 1: 
    if true {x=42} {x=666}    
    -> true; #0 = 42; #1 = 666; empty
    -> #0 = 42; empty
    -> 42; empty
    
    Instruction: IfStmtInstr(Lit(Bool(true)),List(AssignInstr(0,Lit(Num(42)))),List(AssignInstr(0,Lit(Num(666)))))
    Heap: Vector(null)
    
    Instruction: AssignInstr(0,Lit(Num(42)))
    Heap: Vector(null)
    
    Halted
    Heap: Vector(NumValue(42))
    
    
    Example 2: 
    if false {x=42} {x=666}    
    -> true; #0 = 42; #1 = 666; empty
    -> #0 = 666; empty
    -> 666; empty
    
    
    Instruction: IfStmtInstr(Lit(Bool(false)),List(AssignInstr(0,Lit(Num(42)))),List(AssignInstr(0,Lit(Num(666)))))
    Heap: Vector(null)
    
    Instruction: AssignInstr(0,Lit(Num(666)))
    Heap: Vector(null)
    
    Halted
    Heap: Vector(NumValue(666))
    
    
    
    Example 3: 
    a = 1; if (true) {if(false) {x=42} {x=666}} {x=666}
    -> true; false; #0 = 42; #1 = 666; #2 = 666 empty
    -> false; #0 = 42; #1 = 666; empty
    -> #0 = 666; empty
    -> 666; empty
    
    Instruction: IfStmtInstr(Lit(Bool(true)),List(IfStmtInstr(Lit(Bool(false)),List(AssignInstr(0,Lit(Num(42)))),List(AssignInstr(0,Lit(Num(666)))))),List(AssignInstr(0,Lit(Num(666)))))
    Heap: Vector(null)
    
    Instruction: IfStmtInstr(Lit(Bool(false)),List(AssignInstr(0,Lit(Num(42)))),List(AssignInstr(0,Lit(Num(666)))))
    Heap: Vector(null)
    
    Instruction: AssignInstr(0,Lit(Num(666)))
    Heap: Vector(null)
    
    Halted
    Heap: Vector(NumValue(666))
    

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

    P[pc] = i = e
    e, ss1, ss2 => ss1
    -----------------------
    P, pc, h -> P++ss1, pc+1, h

    P[pc] = i = e
    e, ss1, ss2 => ss2
    -----------------------
    P, pc, h -> P++ss2, pc+1, h

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

    Solution:
    See AssignTest.scala:68-98

Task 4: Implement conditionals for the Assign language. We already
extended the expression type with conditionals. Please do not change
any of the signatures of expressions, because this makes it impossible
for us to test your code! To solve this task, add conditionals jumps
to the instructions, extend the compiler to handle conditionals and
extend the runtime of the abstract machine to handle conditional
jumps. Add at least 3 tests to `AssignTest.scala`, that test programs
containing conditionals.

    Solution:
    See Code and Tests.
    The behaviour of the if jumps seems to be correct (result is as expected). 
    Hover some of the test are failing which must be related to the fact of changing states
    within my runtime. I believe there is another way of dealing with the states by not
    adding further instructions the way I do at the moment. 
    Thus, I would appreciate if you could elaborate how to avoid that.

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

    Solution:
    
    expr -> expr'
    ---------------------------------------------
    while expr ss -> while expr' ss
    
    expr -> true
    ---------------------------------------------
    while true ss -> ss'; while expr ss
    
    expr -> false
    ---------------------------------------------
    while false ss -> ;

    Example 1: 
    while false {x=1}    
    -> #0 = 1; empty
    -> empty
    
    Instruction: WhileStmtInstr(Lit(Bool(false)),List(AssignInstr(0,Lit(Num(42)))))
    Heap: Vector(null)
    
    Halted
    Heap: Vector(null)
    
    Example 2: 
    x = 0; while (!(x=1)) {x=x+1}
    -> #0 = 0; #0 = #0 + 1; empty
    -> #0 = 1; empty
    -> empty
        
    Instruction: AssignInstr(0,Lit(Num(0)))
    Heap: Vector(null)
    
    Instruction: WhileStmtInstr(Not(Eq(Var(0),Lit(Num(1)))),List(AssignInstr(0,Lit(Num(1)))))
    Heap: Vector(NumValue(0))
    
    Instruction: AssignInstr(0,Lit(Num(1)))
    Heap: Vector(NumValue(0))
    
    Instruction: WhileStmtInstr(Not(Eq(Var(0),Lit(Num(1)))),List(AssignInstr(0,Lit(Num(1)))))
    Heap: Vector(NumValue(1))
    
    Halted
    Heap: Vector(NumValue(1))
    
    
    Example 3:
    x = 0; while (!(x=2)) {x=x+1}
    -> #0 = 0; #0 = #0 + 1; empty
    -> #0 = 1; #0 = #0 + 1; empty
    -> #0 = 1; empty
    -> empty
    
    Heap: Vector(NumValue(0))
    
    Instruction: AssignInstr(0,Add(Var(0),Lit(Num(1))))
    Heap: Vector(NumValue(0))
    
    Instruction: WhileStmtInstr(Not(Eq(Var(0),Lit(Num(2)))),List(AssignInstr(0,Add(Var(0),Lit(Num(1))))))
    Heap: Vector(NumValue(1))
    
    Instruction: AssignInstr(0,Add(Var(0),Lit(Num(1))))
    Heap: Vector(NumValue(1))
    
    Instruction: WhileStmtInstr(Not(Eq(Var(0),Lit(Num(2)))),List(AssignInstr(0,Add(Var(0),Lit(Num(1))))))
    Heap: Vector(NumValue(2))
    
    Halted
    Heap: Vector(NumValue(2))

# Compiled While language

While loops are also compiled to conditional jumps. The set of
instructions and the abstract machine stay unchanged:

    State ::= (Prog, PC, Heap)
    PC ::= Number
    Heap ::= empty | Literal; Heap

Task 6: Define the semantics of the compiled While language as a state
transition relation. Provide traces for 3 different programs.

    P[pc] = i = e
    e, ss => ss
    -----------------------
    P, pc, h -> P++e++ss, pc+1, h'
    
    
    P[pc] = i = e
    e, ss => ;
    -----------------------
    P, pc, h -> P, pc+1+ss.length, h'

# Implementation of while loops

Task 7: Implement while loops for the Assign language with
conditionals. Use the same code base that you created for task 3. Add
at least 3 tests to `WhileTest.scala`, that test programs containing
while loops.

    Solution:
    See Code and Tests.
    Same problem regarding tests as with conditional jumps remains.

# Security vulnerabilities?

Task 8: Can you spot weaknesses in our model of While that may yield
security vulnerabilities later on? Describe any weakness you can find and
discuss whether it is exploitable or how it would become exploitable.

    The language does not have garbage collection. 
    Thus, the heap could grow and as a result end up in a heap overflow.
    An attempt for exploiting this could be: while loop where each time 
    a new variable gets assigned.