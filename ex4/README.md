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

Task 3: Test your implementation of the type checker by writing unit tests that check type correct and type incorrect programs. Each test has to exercise a particular statement case. (3pt)

Task 4: From the lecture notes: "A static analysis must correctly approximate the actual program behavior." For static type systems, this means that the assigned type must describe all values that an expression may compute at runtime. Discuss: What would happen if a static type system was did not correctly approximate the actual program behavior. Describe an example of such a situation. (2pt)

Task 5: We defined a static type system. But type checking can also be implemented as a dynamic analysis. Sketch the design of a dynamic type-checking analysis. What are the trade-offs between static and dynamic type checking? (1pt)