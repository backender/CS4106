import common.Lang._
import common.Runtime._
import FunLang._
import FunInstrs._

import scala.collection.mutable

object FunCompiler {
  case class FunctionDef(pos: Pos, frameSize: Int)

  type Environment = Map[Variable, Loc]
  type FunctionDefs = Map[Label, FunctionDef]


  def compile(program: Program) : (List[Instruction], Pos, Int) = {
    var pos = 0
    var functions : FunctionDefs = Map()
    var instrs : mutable.MutableList[Instruction] = mutable.MutableList()

    // Compile functions.
    for (func <- program.functions) {
      val startEnv : Environment = func.args.zipWithIndex.toMap

      // Create dummy function entry for recursive calls and deduce frame size.
      functions += (func.label -> FunctionDef(0, 0))
      val frameSize = deduceFrameSize(startEnv, functions, func.body)
      functions += (func.label -> FunctionDef(pos, frameSize))

      val (endPos, _, funcInstrs) = compileStatements(pos, startEnv, functions, func.body)
      instrs ++= funcInstrs
      pos = endPos
    }

    // Compile main.
    val mainEnv : Environment = Map()
    val mainFrameSize = deduceFrameSize(mainEnv, functions, program.main)
    val (_, _, mainInstrs) = compileStatements(pos, mainEnv, functions, program.main)
    instrs ++= mainInstrs

    (instrs.toList, pos, mainFrameSize)
  }

  def deduceFrameSize(startEnv: Environment, functions: FunctionDefs, statements: List[Statement]) : Int = {
    var env = startEnv

    for (statement <- statements) {
      val (_, newEnv, _) = compileStatement(0, env, functions, statement)
      env = newEnv
    }

    env.size
  }

  def compileStatements(startPos: Pos, startEnv: Environment, functions: FunctionDefs, statements: List[Statement]) : (Pos, Environment, List[Instruction]) = {
    var instructions : mutable.MutableList[Instruction] = mutable.MutableList()
    var pos = startPos
    var env = startEnv

    for (statement <- statements) {
      val (endPos, newEnv, instrs) = compileStatement(pos, env, functions, statement)
      pos = endPos
      env = newEnv
      instructions ++= instrs
    }

    (pos, env, instructions.toList)
  }

  def compileStatement(startPos: Pos, startEnv: Environment, functions: FunctionDefs, statement: Statement) : (Pos, Environment, List[Instruction]) = {
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
        val (endPos, newEnv, bodyInstrs) = compileStatements(pos + 1, env, functions, body)

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

      // x = a[i]
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

      // x = foo(2, 6)
      case CallStmt(x, label, args) => {
        val varLoc = lookup(x)
        var argExprs = args.map { x => compileExpr(env, x) }
        val funcDef = functions.get(label) match {
          case Some(t) => t
          case None => sys.error("Function %s not found".format(label))
        }

        instructions += CallInstr(varLoc, funcDef.pos, funcDef.frameSize, argExprs)
        pos += 1
      }

      // return 42
      case ReturnStmt(expr) => {
        instructions += ReturnInstr(compileExpr(env, expr))
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
