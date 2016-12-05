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

Task 2: Explain what each element of the array `newpw` is supposed to do. At wich point in the trace does the exploit override part of the heap that alters the control flow? (3pt)

Task 3: Discuss which data on the heap should be protected against user access to prevent such exploits? (1pt)

# Protected Regions

Task 4: Develop a dynamic analysis that protects those parts of the heap you identified in Task 3. Describe what property the dynamic analysis ensures? (1pt)

Task 5: Define the state transition relation that implements your dynamic analysis from Task 4 using runtime monitoring. (3pt)

Task 6: Implement your dynamic analysis from Task 4 in Scala. For this task, modify the file `SafeFunRuntime.scala` and use the field `protect` to track memory locations that should be protected. (3pt)

Task 7: Sketch how you would realize the same dynamic analysis using compile-time instrumentation. (1pt)

# Protected Regions in Practice

Task 8: Investigate how C, Python, and Java each protect runtime metadata on the heap. (1.5pt)
