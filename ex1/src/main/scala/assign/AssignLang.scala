import common.Lang._

object AssignLang {

  case class Program(statements: List[Statement])

  // Statements
  sealed abstract class Statement
  // Example: x := x + 42
  case class AssignStmt(identifier: Variable, expression: Expression[Variable]) extends Statement
  // Example: if(false) { x := x + 1 } { x := 0 }
  case class IfStmt(expression: Expression[Variable], ifBranch: List[Statement], elseBranch: List[Statement]) extends Statement

  // Expressions
  sealed abstract class Expression[Identifier]
  // Variables
  case class Var[Identifier](identifier: Identifier) extends Expression[Identifier]
  // Literals
  case class Lit[Identifier](literal: Literal) extends Expression[Identifier]
  // Arithmetic Expressions
  case class Neg[Identifier](expression: Expression[Identifier]) extends Expression[Identifier]
  case class Add[Identifier](left: Expression[Identifier], right: Expression[Identifier]) extends Expression[Identifier]
  case class Sub[Identifier](left: Expression[Identifier], right: Expression[Identifier]) extends Expression[Identifier]
  case class Mul[Identifier](left: Expression[Identifier], right: Expression[Identifier]) extends Expression[Identifier]
  case class Div[Identifier](left: Expression[Identifier], right: Expression[Identifier]) extends Expression[Identifier]
  // Boolean Expressions
  case class Not[Identifier](expression: Expression[Identifier]) extends Expression[Identifier]
  case class And[Identifier](left: Expression[Identifier], right: Expression[Identifier]) extends Expression[Identifier]
  case class Or[Identifier](left: Expression[Identifier], right: Expression[Identifier]) extends Expression[Identifier]
  case class Eq[Identifier](left: Expression[Identifier], right: Expression[Identifier]) extends Expression[Identifier]

}
