# Type Checking

Context of this exercise is the typed fun language. Consider the following program:

```
int gcd (int a , int b) {
  if (a == b) {
    return a;
  } else {
    if(a > b) {
      c := gcd(a-b, b);
      return c;
    } else {
      c := gcd(a, b-a);
      return c;
    }
  }
}
```

Task 1: Draw a derivation tree for `gcd` (see lecture notes for type inference rules) to proof that this program is well-typed. (1pt)

                    ---------------------------------           ---------------------------------
                    F, C, R |- return c: Int                    F, C, R |- return c: Int
                    ---------------------------------           ---------------------------------
                    F, c, R | c := call gdb(a-b, b); ss:ok      F, C, R |- c:=call gdb(a, b-a); ss:ok
                                            \                        /
                                             \                      /
    ---------------------------         ---------------------------------
    F, C, R |- return a : Int           C |- if (a > b) ss1; ss2; ss: ok       
    ---------------------------         ---------------------------------
                    \                                   /
                     \       C |- a {==} b : Bool       /
                    -----------------------------------------
                    C |- if (a == b) ss1; ss2; ss:ok
                    -----------------------------------------
                    F, (a: Int; b:Int), R |- ss:ok
                    -----------------------------------------
                    F |- fun R gdb((Int a), (Int b)) ss:ok

Task 2: Develop a complete type system of Fun and implement a type checker. Write down the inference rules of cases that were not provided in the lecture notes. To implement the type checker, extend `FunTypeChecker` in the accompanying code, where we already defined the cases for `arnew` as an example. (5pt)

Familiarise yourself with the `scala.util.Try` monad to avoid extensive pattern matching to handle type checking failure. Example usage:

  def typeCheckExpression(ctx: Context, expr: Expression[Variable]) : Try[Type] = expr match {
    ...
    case Eq(e1,e2) => for {
      t1 <- typeCheckExpression(ctx, e1)
      t2 <- typeCheckExpression(ctx, e2)
      if t1 == t2
    } yield BooleanType()
  }

    Solution:
    
    1. See code.
    
    2. ...
    
    //  Missing Boolean Expressions: <,>
    
    C |- e1 : Bool
    C |- e2 : Bool
    -------------------
    C |- e1 {<,>} e2 : Bool
    
    
    // Missing Expression: Var
    
    x:T; C |- get(C, x) = T
    -------------------
    C |- x : T
    
    
    // Missing Statements
    
    C |- e : Bool
    C |- ss1 : ok
    C |- ss2 : ok
    ----------------
    C |- if e ss1; ss2 : ok
    
    
    C |- e : Int
    C |- a : T[t]
    C |- i : Int
    x:T[t]; C |- ss : ok
    ---------------------
    C |- x := arread T i; ss : ok
    
    C |- e : Int
    C |- a : T[t]
    C |- i : Int
    x:T[t]; C |- ss : ok
    ---------------------
    C |- x := arread T i; ss : ok
    

Task 3: Test your implementation of the type checker by writing unit tests that check type correct and type incorrect programs. Each test has to exercise a particular statement case. (3pt)

    Solution:
    
    1. See Tests.

Task 4: From the lecture notes: "A static analysis must correctly approximate the actual program behavior." For static type systems, this means that the assigned type must describe all values that an expression may compute at runtime. Discuss: What would happen if a static type system was did not correctly approximate the actual program behavior. Describe an example of such a situation. (2pt)

    Not correct approximation of the program behaviour could lead to
    unexpected runtime behaviour. Consider the example where some type `a`
    would be wrongly aproximated to an `Int` instead of a `Bool`.
    Assuming one would append `a` to an array `A` of `Int's` this would then
    work on compile time. However, on runtime one might then access `a` 
    from the Int array A. The runtime would at this point think of `a` being
    an Int whereas in fact it would be a bool and things would crash if
    one would execute `len(a)` or worse...

Task 5: We defined a static type system. But type checking can also be implemented as a dynamic analysis. Sketch the design of a dynamic type-checking analysis. What are the trade-offs between static and dynamic type checking? (1pt)

    Type-checking implemented as a dynamic analysis would require to have 
    a much more flexible Context and Function context as this would also
    require some sort of cleaning of the context as this could grow into
    infinity when the program is constantly running. I presume the actual
    behaviour would look similar to a staic type system, only would the checkings
    be done on runtime. Presumably, the optimization as well as the inferrence
    becomes not that straigtforward as some parts of the code may be unknown yet.
    Perhaps one could look at this as a lazy evaluation of the types over the
    course of the runtime.
    