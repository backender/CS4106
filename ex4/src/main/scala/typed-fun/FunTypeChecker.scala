package typedfun

import common.Lang._
import typedfun.FunLang._

import scala.util.{Failure, Success, Try}

object FunTypeChecker {

  type Context = List[(Variable, Type)]
  type FunctionContext = List[(Variable, FunctionType)]

  var ctx = List() : Context
  var fctx = List() : FunctionContext

  def typeCheckProgram(prog: Program): Try[Unit] = prog match {
    case Program(functions, statements) =>
      fctx = prog.functions.map{
        case Function(returnType, label, args, _) =>
          (label, FunctionType(args.map(a => a.typ), returnType)) : (Variable, FunctionType)
      }
      for {
        _ <- typeCheckStatements(fctx, ctx, statements)
        _ <- typeCheckFunctions(fctx, ctx, functions)
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
    case Function(rt, label, args, body)  =>
        args.foreach( a => extend(ctx, a.variable, a.typ))
        val t = typeCheckStatements(fctx, ctx, body)
        expect(rt, t)
        Success()
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
      _ = (ft.args zip argsT).map{case (a, b) => expect(a, b) }
      //TODO Question: we presume that identifier was not declared previously, e.g. direct assignment, correct?
      //t <- lookupInContext(ctx, identifier)
      //_ <- expect(t, ft.ret)
      rt <- typeCheckStatements(fctx, extend(ctx, identifier, ft.ret), ss)
    } yield rt

    case ReadArrayStmt(identifier, array, index) :: ss => for {
      it <- lookupInContext(ctx, identifier)
      at <- lookupInContext(ctx, array)
      _ <- expect(ArrayType(it), at)
      rt <- typeCheckStatements(fctx, extend(ctx, identifier, it), ss)
    } yield rt

    case ReturnStmt(value) :: ss => for {
      rt <- typeCheckExpression(ctx, value)
      ss = VoidType()
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
    case Lit(literal) => Success(NumberType())
    case Neg(expression) => for {
      t <- typeCheckExpression(ctx, expression)
      _ <- expect(NumberType(), t)
    } yield NumberType()
    case Add(left, right) => typeCheckArithmetic(ctx, left, right)
    case Sub(left, right) => typeCheckArithmetic(ctx, left, right)
    case Mul(left, right) => typeCheckArithmetic(ctx, left, right)
    case Div(left, right) => typeCheckArithmetic(ctx, left, right)
    //case Exp
    case Not(e) => for {
      t <- typeCheckExpression(ctx, e)
      _ <- expect(BooleanType(), t)
    } yield BooleanType()
    case And(left, right) => typeCheckBoolean(ctx, left, right)
    case Or(left, right) => typeCheckBoolean(ctx, left, right)
    case Lt(left, right) => typeCheckBoolean(ctx, left, right)
    case Gt(left, right) => typeCheckBoolean(ctx, left, right)
  }

  def typeCheckArithmetic(ctx: Context, left: Expression[Variable], right: Expression[Variable]) = for {
    l <- typeCheckExpression(ctx, left)
    r <- typeCheckExpression(ctx, right)
    _ <- expect(NumberType(), l)
    _ <- expect(NumberType(), r)
  } yield NumberType()

  def typeCheckBoolean(ctx: Context, left: Expression[Variable], right: Expression[Variable]) = for {
    l <- typeCheckExpression(ctx, left)
    r <- typeCheckExpression(ctx, right)
    _ <- expect(BooleanType(), l)
    _ <- expect(BooleanType(), r)
  } yield BooleanType()

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