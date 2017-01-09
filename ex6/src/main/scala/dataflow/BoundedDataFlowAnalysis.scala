package dataflow

import array.ArrayLang._
import common.Lang._
import dataflow.ControlFlowGraph.CFG

import scala.collection.immutable.Queue

object BoundedDataFlowAnalysis {

  sealed abstract class Num
  case class NumV(x: Int) extends Num
  case class NegInf() extends Num
  case class PosInf() extends Num

  sealed case class Interval(from: Num, to: Num)

  abstract class Property
  case class NumP(interval: Interval) extends Property
  case class ArrP(interval: Interval) extends Property
  case class UnknownP() extends Property

  type AnalysisResult = Map[Variable,Property]

  def min(x:Num,y:Num) : Num = ???

  def max(x:Num,y:Num) : Num = ???

  def joinInterval(iv1: Interval, iv2: Interval) : Interval =
    Interval(min(iv1.from,iv2.from),max(iv1.to,iv2.to))

  def joinProperty(p1: Property, p2: Property) : Property = (p1,p2) match {
    case (NumP(x),NumP(y)) => NumP(joinInterval(x,y))
    case (ArrP(x),ArrP(y)) => ArrP(joinInterval(x,y))
    case _ => UnknownP()
  }

  def join(ar1: AnalysisResult, ar2: AnalysisResult) : AnalysisResult = {
    (ar1.keySet ++ ar2.keySet).foldLeft(Map():AnalysisResult){
      case (res,k) => (ar1.get(k),ar2.get(k)) match {
        case (Some(x),Some(y)) => res + (k -> joinProperty(x,y))
        case (None,Some(y)) => res + (k -> y)
        case (Some(x),None) => res + (k -> x)
        case _ => res
      }
    }
  }

  def joinList(ars: List[AnalysisResult]) : AnalysisResult =
    ars.foldLeft(Map():AnalysisResult)(join)

  def analyzeStatement(ar: AnalysisResult, statement: Statement) : AnalysisResult = statement match {
    case AssignStmt(x,e) => ar + (x -> analyzeExpression(ar,e))
    case ArrayInitStmt(x,vals) => ar + (x -> ArrP(Interval(NumV(vals.length), NumV(vals.length))))
    case NewArrayStmt(x,e) => ar + (x -> (analyzeExpression(ar,e) match {
      case NumP(iv) => ArrP(iv)
      case _ => UnknownP()
    }))
    case ReadArrayStmt(x,a,e) => ar + (x -> UnknownP())
    case AssignArrayStmt(_,_,_) => ar
    case WhileStmt(_,_) => ar
    case IfStmt(_,_,_) => ar
  }

  def analyzeExpression(ar: AnalysisResult, expr: Expression[Variable]) : Property = expr match {
    case Var(x) => ar(x)
    case Lit(Num(n)) => NumP(Interval(NumV(n), NumV(n)))
    case Add(e1,e2) => ???
    case Sub(e1,e2) => ???
    case Mul(e1,e2) => ???
    case Div(e1,e2) => ???
    case Neg(e) => ???
    case ArrayLength(x) => ar(x) match {
      case ArrP(iv) => NumP(iv)
      case _ => UnknownP()
    }
    case _ => UnknownP()
  }

  case class Block(entry: AnalysisResult, exit: AnalysisResult)
  type Analysis = Map[Int,Block]

  def analysis(cfg: CFG) : List[Analysis] =
    iterate(cfg, cfg.nodes.indices.map{i => i -> Block(Map(),Map())}.toMap)

  private def iterate(cfg: CFG, analysis: Analysis) : List[Analysis] = {
    val newAnalysis = iterate(cfg, analysis, Set(), Queue(0))
    if(analysis.hashCode() != newAnalysis.hashCode() || analysis != newAnalysis)
      newAnalysis :: iterate(cfg, newAnalysis)
    else
      List()
  }

  private def iterate(cfg: CFG, analysis:Analysis, visited: Set[Int], workQueue: Queue[Int]) : Analysis =
    if(workQueue.isEmpty) {
      analysis
    } else {
      val (blockIdx,wq) = workQueue.dequeue
      if(visited.contains(blockIdx)) {
        iterate(cfg, analysis, visited, wq)
      } else {
        val predecessorExits =
          if(blockIdx > 0)
            cfg.predecessors(blockIdx).map{pre => analysis(pre).exit}
          else
            List(Map()):List[AnalysisResult]
        val entry = joinList(predecessorExits)
        val exit =
          if(blockIdx < cfg.nodes.length)
            analyzeStatement(entry, cfg.nodes(blockIdx))
          else
            Map():AnalysisResult
        val successors =
          if(blockIdx < cfg.nodes.length)
            cfg.successors(blockIdx)
          else
            List()
        iterate(
          cfg,
          analysis + (blockIdx -> Block(entry, exit)),
          visited + blockIdx,
          wq.enqueue(successors))
      }
    }
}