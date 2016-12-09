# Control Exploit

The context of this assignment is the Fun language.

The Fun language does not separate user data from runtime metadata (e.g., PC, Frame) on the heap. The following program executes code that was never intentionally called and that takes over control.

   newpw = new Array[11] // newpw is entered by the user
   newpw[0] = 18
   newpw[1] = 0
   newpw[2] = 0
   newpw[3] = 0
   newpw[4] = 14
   newpw[6] = 6
   newpw[7] = 8
   pw = new Array[10]
   setpassword(14,pw,newpw)
   grantAdminAccess(1)

   setpassword(userid,pw,newpw) {
     i = len newpw
     while (i >= 0) {
       x = newpw[i]
       pw[i] = x
       i = i - 1
     }
   }

   grantAdminAccess(userid) {
      ...
      return userid
   }

The program is compiled to the following instructions:

       // setpassword
   0:  AssignInstr(3,Sub(ArrayLength(2),Lit(Num(1)))),
   1:  JumpIfInstr(6,Not(Or(Gt(Var(3),Lit(Num(0))),Eq(Var(3),Lit(Num(0)))))),
   2:  ReadArrayInstr(4,2,Var(3)),
   3:  AssignArrayInstr(1,Var(3),Var(4)),
   4:  AssignInstr(3,Sub(Var(3),Lit(Num(1)))),
   5:  JumpIfInstr(1,Lit(Bool(true))),
   6:  ReturnInstr(Var(1)),
       // grantAdminAccess
   7:  ReturnInstr(Var(0)),
       // main
   8:  NewArrayInstr(0,Lit(Num(8))),
   9:  AssignArrayInstr(0, Lit(Num(0)), Lit(Num(18))),
   10: AssignArrayInstr(0, Lit(Num(1)), Lit(Num(0))),
   11: AssignArrayInstr(0, Lit(Num(2)), Lit(Num(0))),
   12: AssignArrayInstr(0, Lit(Num(3)), Lit(Num(0))),
   13: AssignArrayInstr(0, Lit(Num(4)), Lit(Num(14))),
   14: AssignArrayInstr(0, Lit(Num(6)), Lit(Num(6))),
   15: AssignArrayInstr(0, Lit(Num(7)), Lit(Num(8))),
   16: NewArrayInstr(1,Lit(Num(6))),
   17: CallInstr(2,0,5,List(Lit(Num(14)), Var(1), Var(0))),
   18: CallInstr(2,7,1,List(Lit(Num(1))))

The archive of this exercise contains a CSV file `trace.csv` that contains trace of running the program. You can conveniently view the CSV file using your favorite spreadsheet program (Excel, Numbers, Gnumeric, etc.).

Task 1: The exploit takes over the control of the program. Which function is unexpectedly executed? What are the arguments to this function? (1pt)

    Solution:
    Step #54: free(h, 4, 7)

Task 2: Explain what each element of the array `newpw` is supposed to do. At wich point in the trace does the exploit override part of the heap that alters the control flow? (3pt)

    Solution:
    Each element of `newpw` should override the corresponding index of `pw`. However, since the lenght of `newpw`is longer than `pw` we see unexpected behaviour at step #15 and #20 (indictated with red color). Concretely, FP and PC are being overridden with the values of `newpw[7]` and `newpw[6]` respectively. The modification of the frame pointer leads to freeing of values upon function return. For an illustration of my thoughts please see the trace.pdf file where I marked steps with colors.

See [trace.pdf](trace.pdf) for more details.

Task 3: Discuss which data on the heap should be protected against user access to prevent such exploits? (1pt)

    Solution:

    Registers should be protected to write unless its done by the runtime
    where a call or return instruction gets executed, in this case, registers
    can be added or removed from the heap.


# Protected Regions

Task 4: Develop a dynamic analysis that protects those parts of the heap you identified in Task 3. Describe what property the dynamic analysis ensures? (1pt)

    Solution:

    Given a set of protected memory locations, we will add the first
    register (location 0-3) and every upcoming register, created on a
    function call, to the set of protected locations. Vice-versa, on
    returning a function, the memory locations corresponding to the
    registers of that function are being freed from the protected set.
    To ensure that those locations are handled in read-only fashion,
    every statement that is about to proceed a write on an array
    location (AssignArrayInstr) is being checked whether this location
    is protected or not.

Task 5: Define the state transition relation that implements your dynamic analysis from Task 4 using runtime monitoring. (3pt)

    Solution:

    P[pc] = rt' := call pc' fs' (e_0 ... e_n)
    // evaluate all arguments first
    e_i, fp, h => lit_i   forall 0 <= i <= n
    // allocate new stack frame, reserve space to store registers
    alloc(h, fs' + 4) = (h1, loc)
    // store registers
    put(h1, loc+0, pc) = h2
    put(h2, loc+1, fp) = h3
    put(h3, loc+2, fs) = h4
    put(h4, loc+3, rt) = h5

    prot’ = prot ++ [fp+0, fp+1, fp+2, fp+3]

    // new stack frame begins after stored register
    fp' = loc + 4
    // store function arguments in stack frame
    put(h{5+i}, fp'+i, lit_i) = h{5+i+1}    forall 0 <= i <= n
    // Continue execution at pc' with stack frame fp', "hiding" the stored registers.
    -------------------------------------------------------------
    P, (pc, fp, fs, rt), h, prot -> P, (pc', fp', fs', rt'), h{5+n+1}, prot'


    P[pc] = return e
    e, fp, h => lit
    // restore registers
    get(h, fp-4) = pc'
    get(h, fp-3) = fp'
    get(h, fp-2) = fs'
    get(h, fp-1) = rt'
    4
    // store function return value in old stack frame
    put(h, fp'+rt, lit) = h'
    // free current stack frame, including stored registers
    free(h', fp - 4, fp + fs - 1) = h’'

    prot’ = prot -- [fp-1, fp-2, fp-3, fp-4]
    -------------------------------------------------------
    P, (pc, fp, fs, rt), h, prot -> P, (pc'+1, fp' fs', rt'), h’’, prot’


    P[pc] = arwrite a e1 e2
    e1, fp, h => lit1
    e2, fp, h => lit2

    get(prot, start+lit1+1) == {} //check if location of lit1 is not protected

    // retrieve the start of the array `a`
    get(h, fp+a) = start
    // store value `lit2` in array `a` at index `lit1`, skipping the array header
    put(h, start + lit1 + 1, lit2) = h'
    -----------------------
    P, (pc, fp, fs, rt), h, prot -> P, (pc+1, fp, fs, rt), h', prot

    // Assuming that those are preconditions, we do not have to handle the case where get(prot, start+lit1+1) != {}


Task 6: Implement your dynamic analysis from Task 4 in Scala. For this task, modify the file `SafeFunRuntime.scala` and use the field `protect` to track memory locations that should be protected. (3pt)


    Solution:

    See SafeFunRuntime and passing tests.

Task 7: Sketch how you would realize the same dynamic analysis using compile-time instrumentation. (1pt)

    Solution:

    On compile time one could proceed bound checking for arrays and therefore
    prevent the case where registers are overriden. Concretely, in
    AssignArrayStmt one would check whether the index is within the range
    of varloc(x) and varloc(x)+size-of-x. If this condition is false, one would prevent
    this action.

# Protected Regions in Practice

Task 8: Investigate how C, Python, and Java each protect runtime metadata on the heap. (1.5pt)

    Solution:

    C ->
    Python ->
    Java ->
