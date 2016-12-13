package typedfun

import common.Lang._
import typedfun.FunLang._
import scala.util.{Try, Success, Failure}

object FunTypeChecker {

  type Context = List[(Variable, Type)]
  type FunctionContext = List[(Variable, FunctionType)]

  def typeCheckProgram(prog: Program): Try[Unit] = ???

  def typeCheckFunctions(fctx: FunctionContext, ctx: Context, functions: List[Function]): Try[Unit] = functions match {
    case f :: fs => for {
      _ <- typeCheckFunction(fctx,ctx,f)
      _ <- typeCheckFunctions(fctx,ctx,fs)
    } yield ()
    case List() => Success()
  }

  def typeCheckFunction(fctx: FunctionContext, ctx: Context, fun: Function): Try[Unit] = ???

  def typeCheckStatements(fctx: FunctionContext, ctx: Context, statements: List[Statement]): Try[Type] = statements match {
    case NewArrayStmt(a, ty, l) :: ss => for {
      _  <- ensureNum(ctx, l)
      rt <- typeCheckStatements(fctx, extend(ctx, a, ArrayType(ty)), ss)
    } yield rt
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