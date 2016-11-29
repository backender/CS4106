package array

import common.Lang._
import common.Runtime._

import array.ArrayLang._
import array.ArrayInstrs._

import scala.collection.mutable

class SafeArrayCompiler extends ArrayCompiler {

  // TODO: modify the `compileStatement` method.
  override def compileStatement(startPos: Pos, startEnv: Environment, statement: Statement) : (Pos, Environment, List[Instruction]) = {
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
        env.get(a) match {
          case Some(-1) =>
            instructions += AbortInstr("Unable to free more than once.")
          case _ =>
            instructions += DeleteArrayInstr(arrLoc)
            env += (a -> -1) // mark array location as free
        }
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

}