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

          val startBody = line + 1
          val endBody = line + flatten(body).length
          val nextStatement = endBody + 1

          if(statements.tail.isEmpty) {
            val cfgPrime = CFG(
              statements,
              Map(line -> List(startBody), endBody -> List(line)) |+| whileCFG.successors,
              Map(startBody -> List(line), line -> List(endBody)) |+| whileCFG.predecessors
            )
            println(line + ": " + cfgPrime)
            (cfgPrime, line)
          } else {
            val cfgPrime = CFG(
              statements,
              Map(line -> List(startBody, nextStatement), endBody -> List(line)) |+| whileCFG.successors,
              Map(nextStatement -> List(line), startBody -> List(line), line -> List(endBody)) |+| whileCFG.predecessors
            )
            println(line + ": " + cfgPrime)
            (cfgPrime, line)
          }

        case IfStmt(e, ifBranch, elseBranch) =>
          println("IF-Statement")

          val startIf = line + 1
          val endIf = line + flatten(ifBranch).length
          val startElse = endIf + 1
          val endElse = endIf + flatten(elseBranch).length
          val nextStatement = endElse + 1

          if (statements.tail.isEmpty) {
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
              Map(line -> List(startIf, startElse)) |+| ifCFG.successors |+| elseCFG.successors,
              Map(startIf -> List(line), startElse -> List(line)) |+| ifCFG.predecessors |+| elseCFG.predecessors
            )

            println(line + ": " + cfgPrime)
            (cfgPrime, line)

          } else {
            val ifCFG = controlFlowGraph(
              ifBranch,
              CFG(ifBranch, cfg.successors, cfg.predecessors),
              line + 1)._1

            val elseCFG = controlFlowGraph(
              elseBranch,
              CFG(elseBranch, cfg.successors, cfg.predecessors),
              line + 1 + flatten(ifBranch).length)._1

            val cfgPrime = CFG(
              statements,
              Map(line -> List(startIf, startElse), endIf -> List(nextStatement), endElse -> List(nextStatement)) |+| ifCFG.successors |+| elseCFG.successors,
              Map(startIf -> List(line), startElse -> List(line), nextStatement -> List(endIf, endElse)) |+| ifCFG.predecessors |+| elseCFG.predecessors
            )

            println(line + ": " + cfgPrime)
            (cfgPrime, line)
          }

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