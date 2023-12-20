package aoc

import scala.collection.mutable

object Day20 extends App with Common {

  def parseInput(input: Seq[String]): Map[String, List[String]] =
    input.map { case s"$a -> $b" => (a, b.split(", ").toList) }.toMap

  sealed trait ModuleType

  case object FlipFlop extends ModuleType
  case object Conjunction extends ModuleType
  case object Broadcast extends ModuleType

  lazy val s = Set("ft", "jz", "sv", "ng")

  lazy val cycles = mutable.HashMap[String, List[Long]]()

  def simulate(
      graph: Map[String, List[String]],
      nodeType: Map[String, ModuleType],
      nodeState: mutable.HashMap[String, Boolean],
      memory: Map[String, mutable.HashMap[String, Boolean]],
      i: Long = 0
  ): (Long, Long) = {

    var lowCount, highCount = 0L

    val q = mutable.Queue[(String, String, Boolean)](("button", "broadcaster", false))

    while (q.nonEmpty) {
      val (sender, node, pulse) = q.dequeue()

      val signal = (nodeType.get(node), pulse) match {
        case (Some(FlipFlop), false) =>
          nodeState(node) = !nodeState(node)
          Some(nodeState(node))
        case (Some(Conjunction), _) =>
          memory(node)(sender) = pulse
          if (memory(node).forall(_._2)) {
            nodeState(node) = true
            Some(false)
          } else {
            if (s.contains(node)) cycles.update(node, i :: cycles.getOrElse(node, List.empty))
            nodeState(node) = false
            Some(true)
          }
        case (Some(Broadcast), _) => Some(pulse)
        case _                    => None
      }

      for { s <- signal; c <- graph.get(node).toList.flatten } {
        q.enqueue((node, c, s))
      }

      pulse match {
        case true  => highCount += 1
        case false => lowCount += 1
      }

    }

    (lowCount, highCount)
  }

  def partOne(input: Seq[String]): Long = {
    val data = parseInput(input)

    val nodeType = data.map {
      case (s"%$n", _) => n -> FlipFlop
      case (s"&$n", _) => n -> Conjunction
      case (n, _)      => n -> Broadcast
    }

    val node = data.map {
      case (s"%$n", v) => n -> v
      case (s"&$n", v) => n -> v
      case (k, v)      => k -> v
    }

    val nodeState = mutable.HashMap[String, Boolean]()
    nodeType.foreach {
      case (k, _) => nodeState(k) = false
      case _      => ()
    }

    val memory = nodeType.collect { case (k, Conjunction) =>
      val m = mutable.HashMap[String, Boolean]()
      node.filter(_._2.contains(k)).foreach { case (p, _) => m.update(p, false) }
      k -> m
    }

    val (l, h) = (0 until 1000)
      .map(_ => simulate(node, nodeType, nodeState, memory))
      .reduce((a, b) => (a._1 + b._1, a._2 + b._2))

    println((l, h))

    l * h
  }

  def partTwo(input: Seq[String]): Long = {
    val data = parseInput(input)

    val nodeType = data.map {
      case (s"%$n", _) => n -> FlipFlop
      case (s"&$n", _) => n -> Conjunction
      case (n, _)      => n -> Broadcast
    } ++ Map("rx" -> FlipFlop)

    val node = data.map {
      case (s"%$n", v) => n -> v
      case (s"&$n", v) => n -> v
      case (k, v)      => k -> v
    } ++ Map("rx" -> List.empty)

    val nodeState = mutable.HashMap[String, Boolean]()
    nodeType.foreach {
      case (k, _) => nodeState(k) = false
      case _      => ()
    }

    val memory = nodeType.collect { case (k, Conjunction) =>
      val m = mutable.HashMap[String, Boolean]()
      node.filter(_._2.contains(k)).foreach { case (p, _) => m.update(p, false) }
      k -> m
    }

    var i = 0L

    s.foreach(c => cycles.update(c, List.empty))

    while (!nodeState("rx") && i < 10000) {
      simulate(node, nodeType, nodeState, memory, i)
      i += 1
    }

    def gcd(a: Long, b: Long): Long = if (b == 0) a else gcd(b, a % b)

    cycles.values
      .map(_.sliding(2).toSeq.head match { case Seq(a, b) => a - b })
      .reduce((a, b) => a * b / gcd(a, b))
  }

  println("Part 1: " + partOne(problemInput))
  println("Part 2: " + partTwo(problemInput))
}
