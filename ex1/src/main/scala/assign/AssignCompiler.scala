import common.Lang._
import common.Runtime._

import AssignLang._
import AssignInstrs._

import scala.collection.mutable

object AssignCompiler {

  type Environment = Map[Variable, Loc]
  var environment : Environment = Map()

  def compile(program: Program) : (List[Instruction], Int) = {
    environment = Map()
    (program.statements.flatMap(compileStatement),environment.size)
  }

  def compileStatement(statement: Statement): List[Instruction] = {
    statement match {
      case AssignStmt(ident, expr) => {

        var index = 0
        environment.get(ident) match {
          case Some(i) => index = i
          case None => {
            index = environment.size
            environment += (ident -> index)
          }
        }

        List(AssignInstr(index, compileExpr(environment, expr)))
      }

      // TODO: Handle the case of IfStmt's
      case IfStmt(expression, ifBranch, elseBranch) => {
        val ifComp = ifBranch.map(compileStatement)
        val elseComp = elseBranch.map(compileStatement)
        List(IfStmtInstr(compileExpr(environment, expression), ifComp.flatten, elseComp.flatten))
      }
    }
  }

  def compileExpr(env: Environment, expr: Expression[Variable]) : Expression[Loc] =
    expr match {
      case Var(ident) => env.get(ident) match {
        case Some(index) => Var(index)
        case None => sys.error("Identifier %s not in scope".format(ident))
      }

      case Lit(n) => Lit(n)

      case Neg(e) => Neg(compileExpr(env,e))
      case Add(e1,e2) => Add(compileExpr(env,e1),compileExpr(env,e2))
      case Sub(e1,e2) => Sub(compileExpr(env,e1),compileExpr(env,e2))
      case Mul(e1,e2) => Mul(compileExpr(env,e1),compileExpr(env,e2))
      case Div(e1,e2) => Div(compileExpr(env,e1),compileExpr(env,e2))

      case Not(e) => Not(compileExpr(env,e))
      case And(e1,e2) => And(compileExpr(env,e1),compileExpr(env,e2))
      case Or(e1,e2) => Or(compileExpr(env,e1),compileExpr(env,e2))
      case Eq(e1,e2) => Eq(compileExpr(env,e1),compileExpr(env,e2))

    }
}
