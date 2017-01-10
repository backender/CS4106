package dataflow

import array.ArrayLang._
import scalaz.Scalaz._

object ControlFlowGraph {

  sealed case class CFG(nodes: List[Statement],
                        successors: Map[Int,List[Int]],
                        predecessors: Map[Int,List[Int]]) {
    def +(from: Int, to: Int) = {
      val sl: List[Int] = successors.get(from).getOrElse(List())
      val pl: List[Int] = predecessors.get(to).getOrElse(List())
      CFG(nodes,
        if(! sl.contains(to))
          successors + (from -> (to :: sl))
        else
          successors,
        if(! pl.contains(from))
          predecessors + (to -> (from :: pl))
        else
          predecessors)
    }
  }

  def flatten(statements: List[Statement]) : List[Statement] = statements match {
    case WhileStmt(e,body) :: ss => List(WhileStmt(e,body)) ++ flatten(body) ++ flatten(ss)
    case IfStmt(e,ifBranch,elseBranch) :: ss =>
      List(IfStmt(e,ifBranch,elseBranch)) ++
        flatten(ifBranch) ++ flatten(elseBranch) ++ flatten(ss)
    case stmt :: ss => stmt :: flatten(ss)
    case List() => List()
  }

  def controlFlowGraph(statements: List[Statement]) : CFG =
    controlFlowGraph(statements, CFG(flatten(statements), Map(), Map()),0)._1

  private def controlFlowGraph(statements: List[Statement], cfg: CFG, line: Int) : (CFG,Int) = {

    if(statements.isEmpty) {
      println(line + ": " + cfg)
      (cfg, line)
    } else {

      statements.head match {
        case WhileStmt(e,body) =>
          println("WHILE-Statement")
          val whileCFG = controlFlowGraph(
            body,
            CFG(body, cfg.successors, cfg.predecessors),
            line + 1)._1

          if(statements.tail.isEmpty) {
            val cfgPrime = CFG(
              statements,
              Map(line -> List(line + 1), line+body.length -> List(line)) |+| whileCFG.successors,
              Map(line + 1 -> List(line), line -> List(line + body.length)) |+| whileCFG.predecessors
            )
            println(line + ": " + cfgPrime)
            (cfgPrime, line)
          } else {
            val cfgPrime = CFG(
              statements,
              Map(line -> List(line + 1, line + 1 + body.length), line+body.length -> List(line)) |+| whileCFG.successors,
              Map(line + 1 + body.length -> List(line), line + 1 -> List(line), line -> List(line + body.length)) |+| whileCFG.predecessors
            )
            println(line + ": " + cfgPrime)
            (cfgPrime, line)
          }

        case IfStmt(e, ifBranch, elseBranch) =>
          println("IF-Statement")
          val ifCFG = controlFlowGraph(
            ifBranch,
            CFG(ifBranch, cfg.successors, cfg.predecessors),
            line + 1)._1

          val elseCFG = controlFlowGraph(
            elseBranch,
            CFG(elseBranch, cfg.successors, cfg.predecessors),
            line + 1 + ifBranch.length)._1

          val cfgPrime = CFG(
            statements,
            Map(line -> List(line + 1, line + 1 + ifBranch.length)) |+| ifCFG.successors |+| elseCFG.successors,
            Map(line + 1 + ifBranch.length -> List(line), line + 1 -> List(line)) |+| ifCFG.predecessors |+| elseCFG.predecessors
          )

          println(line + ": " + cfgPrime)
          (cfgPrime, line)

        case stmt =>
          println("Statement")
          if(statements.tail.isEmpty) {
            val cfgPrime = CFG(statements, cfg.successors, cfg.predecessors)
            println(line + ": " + cfgPrime)
            (cfgPrime, line)
          } else {
            val next = controlFlowGraph(
              statements.tail,
              CFG(cfg.nodes, cfg.successors, cfg.predecessors),
              line + 1)._1
            val cfgPrime =
              CFG(
                statements,
                next.successors + (line -> List(line + 1)),
                next.predecessors + (line + 1 -> List(line))
              )
            println(line + ": " + cfgPrime)
            (cfgPrime, line)
          }

      }

    }


  }
}