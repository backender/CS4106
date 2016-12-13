package typedfun

import common.Lang._
import typedfun.FunLang._
import scala.util.{Try, Success, Failure}

object FunTypeChecker {

  type Context = List[(Variable, Type)]
  type FunctionContext = List[(Variable, FunctionType)]

  def typeCheckProgram(prog: Program): Try[Unit] = prog match {
    case Program(functions, statements) => for {

      _ <- typeCheckFunctions(List(), List(), functions)
      _ <- typeCheckStatements(List(), List(), statements)
    } yield ()
  }

  def typeCheckFunctions(fctx: FunctionContext, ctx: Context, functions: List[Function]): Try[Unit] = functions match {
    case f :: fs => for {
      _ <- typeCheckFunction(fctx,ctx,f)
      _ <- typeCheckFunctions(fctx,ctx,fs)
    } yield ()
    case List() => Success()
  }

  def typeCheckFunction(fctx: FunctionContext, ctx: Context, fun: Function): Try[Unit] = fun match {
    case Function(rt, label, args, body)  => for {
      t <- typeCheckStatements(fctx, ctx, body)
      _ <- expect(rt, t)
    } yield ()
  }

  def typeCheckStatements(fctx: FunctionContext, ctx: Context, statements: List[Statement]): Try[Type] = statements match {
    case NewArrayStmt(a, ty, l) :: ss => for {
      _  <- ensureNum(ctx, l)
      rt <- typeCheckStatements(fctx, extend(ctx, a, ArrayType(ty)), ss)
    } yield rt

    case AssignStmt(identifier, expression) :: ss => for {
      t <- typeCheckExpression(ctx, expression)
      rt <- typeCheckStatements(fctx, extend(ctx, identifier, t), ss)
    } yield rt

    case WhileStmt(expression, body) :: ss => for {
      _ <- ensureBool(ctx, expression)
      _ <- typeCheckExpression(ctx, expression)
      rt <- typeCheckStatements(fctx, ctx, body ++ ss)
    } yield rt

    case IfStmt(expression, ifBranch, elseBranch) :: ss => for {
      _ <- ensureBool(ctx, expression)
      _ <- typeCheckExpression(ctx, expression)
      rt <- typeCheckStatements(fctx, ctx, ifBranch ++ elseBranch ++ ss)
    } yield rt

    case AssignArrayStmt(identifier, index, value) :: ss => for {
      _ <- lookupInContext(ctx, identifier)
      t <- ensureArray(ctx, identifier)
      te <- typeCheckExpression(ctx, value)
      _ <- expect(t, ArrayType(te))
      rt <- typeCheckStatements(fctx, ctx, ss)
    } yield rt

    case CallStmt(identifier, name, args) :: ss => for {
      ft <- lookupInFunctionContext(fctx, name)
      argsT <- typeCheckExpressions(ctx, args)
      //_ <- (ft.args zip argsT).map{case (a, b) => expect(a, b) } // TODO: DAMN!!!!
      t <- lookupInContext(ctx, identifier)
      _ <- expect(t, ft.ret)
      rt <- typeCheckStatements(fctx, extend(ctx, identifier, t), ss)
    } yield rt

    case ReadArrayStmt(identifier, array, index) :: ss => for {
      it <- lookupInContext(ctx, identifier)
      at <- lookupInContext(ctx, array)
      _ <- expect(ArrayType(it), at)
      rt <- typeCheckStatements(fctx, extend(ctx, identifier, it), ss)
    } yield rt

    case ReturnStmt(value) :: ss => for {
      rt <- typeCheckExpression(ctx, value)
      // TODO question: is it neccessary to further check the remaining statements? if yes, how?
    } yield rt

    case List() => Success(VoidType())
  }

  def typeCheckExpressions(ctx: Context, exprs: List[Expression[Variable]]) : Try[List[Type]] = exprs match {
    case e :: es => for {
      t <- typeCheckExpression(ctx,e)
      ts <- typeCheckExpressions(ctx,es)
    } yield t :: ts
    case List() => Success(List())
  }

  def typeCheckExpression(ctx: Context, expr: Expression[Variable]) : Try[Type] = expr match {

    case Var(x) => lookupInContext(ctx,x)

    case Eq(e1,e2) => for {
      t1 <- typeCheckExpression(ctx, e1)
      t2 <- typeCheckExpression(ctx, e2)
      if t1 == t2
    } yield BooleanType()
    //case ArrayLength
    //case Lit
    //case Neg
    //case Add
    //case Sub
    //case Mul
    //case Div
    //case Exp
    //case Not
    //case And
    //case Or
    //case Lt
    //case Gt
  }

  case class TypeError(msg: String) extends Exception(msg)

  def typeError[T](msg: String) : Try[T] = Failure(TypeError(msg))

  def expect[T](expected: T, actual: T): Try[T] =
    if (actual == expected) Success(actual)
    else typeError(s"Expected ${expected} but got ${actual}")

  def ensureArray(ctx: Context, a: Variable) =
    lookupInContext(ctx,a).flatMap {
      case ArrayType(t) => Success(t)
      case t => typeError(s"Expected array type, but got ${t}")
    }

  def ensureBool(ctx: Context, e: Expression[Variable]) : Try[Type] =
    typeCheckExpression(ctx,e).flatMap{
      case BooleanType() => Success(BooleanType())
      case t => typeError(s"Expected boolean, but got ${t}")
    }

  def ensureNum(ctx: Context, e: Expression[Variable]) : Try[Type] =
    typeCheckExpression(ctx,e).flatMap{
      case NumberType() => Success(NumberType())
      case t => typeError(s"Expected number, but got ${t}")
    }

  def extend(ctx: Context, xs: List[(Variable,Type)]) = xs ++ ctx
  def extend(ctx: Context, x: Variable, t:Type) : Context = (x,t) :: ctx

  def lookupInFunctionContext(ctx: FunctionContext, f: Variable) : Try[FunctionType] = lookup(ctx,f) match {
    case Some(t) => Success(t)
    case None => typeError(s"function ${f} not in scope")
  }

  def lookupInContext(ctx: Context, x: Variable) : Try[Type] = lookup(ctx,x) match {
    case Some(t) => Success(t)
    case None => typeError(s"variable ${x} not in scope")
  }

  def lookup[T](ctx: List[(Variable,T)], x: Variable) : Option[T] = ctx match {
    case (y,typ) :: _    if x == y => Some(typ)
    case (y,typ) :: rest if x != y => lookup(rest,x)
    case List() => None
  }

}