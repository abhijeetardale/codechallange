package com.intenthq.challenge

import scala.annotation.tailrec

case class Node(value: Int, edges: List[Node] = List.empty)

object SConnectedGraph {

  // Find if two nodes in a directed graph are connected.
  // Based on http://www.codewars.com/kata/53897d3187c26d42ac00040d
  // For example:
  // a -+-> b -> c -> e
  //    |
  //    +-> d
  // run(a, a) == true
  // run(a, b) == true
  // run(a, c) == true
  // run(b, d) == false
  def run(source: Node, target: Node): Boolean = {
    if (source.equals(target)) true
    else {
      //source.edges.flatMap(_.edges).contains(target)
      getConnectedNodes(source, target)
    }
  }


  def getConnectedNodes(source: Node, target: Node): Boolean = {
    @tailrec
    def recrusiveScan(nodes: List[Node], allNodes: List[Node]): List[Node] = {
      nodes match {
        case Nil => allNodes
        case head :: tail => recrusiveScan(tail ++ head.edges, allNodes :+ head)
      }
    }

    recrusiveScan(source.edges, Nil).contains(target)
  }
}
