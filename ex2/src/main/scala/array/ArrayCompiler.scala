package array

import common.Lang._
import common.Runtime._

import array.ArrayLang._
import array.ArrayInstrs._

import scala.collection.mutable

class ArrayCompiler {

  type Environment = Map[Variable, Loc]

  def compile(program: Program) : (List[Instruction], Int) = {
    val (_, env, instructions) = compileStatements(0, Map(), program.statements)
    (instructions,env.size)
  }

  def compileStatements(startPos: Pos, startEnv: Environment, statements: List[Statement]) : (Pos, Environment, List[Instruction]) = {
    var instructions : mutable.MutableList[Instruction] = mutable.MutableList()
    var pos = startPos
    var env = startEnv

    for (statement <- statements) {
      val (endPos, newEnv, instrs) = compileStatement(pos, env, statement)
      pos = endPos
      env = newEnv
      instructions ++= instrs
    }

    (pos, env, instructions.toList)
  }

  def compileStatement(startPos: Pos, startEnv: Environment, statement: Statement) : (Pos, Environment, List[Instruction]) = {
    var instructions : mutable.MutableList[Instruction] = mutable.MutableList()
    var pos = startPos
    var env = startEnv

    def lookup(ident: Variable): Loc = env.get(ident) match {
      case Some(i) => i
      case None => {
        val index = env.size
        env += (ident -> index)
        index
      }
    }
    def lookupFail(ident: Variable): Loc = env.get(ident) match {
      case Some(i) => i
      case None => sys.error("Variable %s has not been initialized".format(ident))
    }

    statement match {
      // x := e
      case AssignStmt(x, expr) => {
        // Fetch the index of the current variable if available or generate a fresh index if needed.
        val varLoc = lookup(x)
        instructions += AssignInstr(varLoc, compileExpr(env, expr))
        pos += 1
      }

      // while (e) { statements }
      case WhileStmt(expr, body) => {
        // Translate while into:
        //    JumpIf(!cond, end)
        //    .. body ..
        //    JumpIf(true, begin)
        val (endPos, newEnv, bodyInstrs) = compileStatements(pos + 1, env, body)

        instructions += JumpIfInstr(endPos + 1, compileExpr(env, Not(expr)))
        instructions ++= bodyInstrs
        instructions += JumpIfInstr(pos, compileExpr(env, Lit(Bool(true))))

        env = newEnv
        pos = endPos + 1
      }

      // x := new Array[len]
      case NewArrayStmt(x, len) => {
        val varLoc = lookup(x)
        instructions += NewArrayInstr(varLoc, compileExpr(env, len))
        pos += 1
      }

      // delete(a)
      case DeleteArrayStmt(a) => {
        val arrLoc = lookupFail(a)
        instructions += DeleteArrayInstr(arrLoc)
        pos += 1
      }

      // x := a[i]
      case ReadArrayStmt(x, a, index) => {
        var varLoc = lookup(x)
        var arrLoc = lookupFail(a)
        instructions += ReadArrayInstr(varLoc, arrLoc, compileExpr(env, index))
        pos += 1
      }

      // x[i] := e
      case AssignArrayStmt(x, index, expr) => {
        val varLoc = lookupFail(x)
        instructions += AssignArrayInstr(varLoc, compileExpr(env, index), compileExpr(env, expr))
        pos += 1
      }

    }

    (pos, env, instructions.toList)
  }

  def compileExpr(env: Environment, expr: Expression[Variable]) : Expression[Loc] = {
    expr match {
      case Var(ident) => env.get(ident) match {
        case Some(index) => Var(index)
        case None => sys.error("Identifier %s not in scope".format(ident))
      }

      case ArrayLength(ident) => env.get(ident) match {
        case Some(index) => ArrayLength(index)
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
      case Lt(e1,e2) => Lt(compileExpr(env,e1),compileExpr(env,e2))
      case Gt(e1,e2) => Gt(compileExpr(env,e1),compileExpr(env,e2))
    }
  }
}
