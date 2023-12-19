package aoc

import scala.annotation.tailrec
import scala.util.control.Breaks._

object Day19 extends App with Common {

  /*
  Example data

  px{a<2006:qkq,m>2090:A,rfg}
  pv{a>1716:R,A}
  lnx{m>1548:A,A}
  rfg{s<537:gd,x>2440:R,A}
  qs{s>3448:A,lnx}
  qkq{x<1416:A,crn}
  crn{x>2662:A,R}
  in{s<1351:px,qqz}
  qqz{s>2770:qs,m<1801:hdj,R}
  gd{a>3333:R,R}
  hdj{m>838:A,pv}

  {x=787,m=2655,a=1222,s=2876}
  {x=1679,m=44,a=2067,s=496}
  {x=2036,m=264,a=79,s=2244}
  {x=2461,m=1339,a=466,s=291}
  {x=2127,m=1623,a=2188,s=1013}
   */

  def parseInput(input: Seq[String]): (Map[String, Workflow], Seq[Map[String, Int]]) =
    input.mkString("\n").split("\n\n").toList match {
      case List(workflows, parts) =>
        workflows.split("\n").toList.map(parseWorkflows).toMap ->
          parts.split("\n").toList.map(parsePart)
    }

  def parsePart(part: String): Map[String, Int] =
    part match {
      case s"{$conditions}" =>
        conditions.split(",").map { case s"$k=$v" => (k, v.toInt) }.toMap
    }

  case class Workflow(conditions: List[(String, Range, String)])

  def parseWorkflows(workflow: String): (String, Workflow) =
    workflow match {
      case s"$name{$conditions}" =>
        name -> Workflow(conditions.split(",").map(parseCondition).toList)
    }

  def parseCondition(condition: String): (String, Range, String) = condition match {
    case s"$key<$limit:$next" => (key, Range(1, limit.toInt), next)
    case s"$key>$limit:$next" => (key, Range(limit.toInt + 1, 4001), next)
    case next                 => ("", Range(1, 4001), next)
  }

  def partOne(input: Seq[String]): Long = {
    val (workflows, parts) = parseInput(input)
    parts
      .map(_.map { case (k, v) => k -> Range(v, v + 1) })
      .filter(p => countAcceptedWorkflow(workflows("in").conditions, workflows, p) == 1)
      .map(_.values.map(_.start).sum)
      .sum
  }

  def countPermutations(parts: Map[String, Range]): Long = parts.map(_._2.size).product

  def countAcceptedWorkflow(
      workflow: List[(String, Range, String)],
      workflows: Map[String, Workflow],
      parts: Map[String, Range]
  ): Long = workflow match {
    case (_, _, "A") :: Nil => countPermutations(parts)
    case (_, _, "R") :: Nil => 0L
    case (_, _, n) :: Nil   => countAcceptedWorkflow(workflows(n).conditions, workflows, parts)
    case (p, r, "A") :: t =>
      val intersection = parts(p).intersection(r)
      val complement = parts(p).relativeComplement(r).filter(_.nonEmpty)
      complement.map(newr => countAcceptedWorkflow(t, workflows, parts.updated(p, newr))).sum +
        intersection.map(i => countPermutations(parts.updated(p, i))).sum
    case (p, r, "R") :: t =>
      val complement = parts(p).relativeComplement(r).filter(_.nonEmpty)
      complement.map(newr => countAcceptedWorkflow(t, workflows, parts.updated(p, newr))).sum
    case (p, r, n) :: t =>
      val intersection = parts(p).intersection(r)
      val complement = parts(p).relativeComplement(r).filter(_.nonEmpty)
      complement.map(newr => countAcceptedWorkflow(t, workflows, parts.updated(p, newr))).sum +
        intersection
          .map(i =>
            countAcceptedWorkflow(workflows(n).conditions.toList, workflows, parts.updated(p, i))
          )
          .sum
  }

  def partTwo(input: Seq[String]): Long = {
    val (workflows, _) = parseInput(input)

    val parts = Map(
      "x" -> Range(1, 4001),
      "m" -> Range(1, 4001),
      "a" -> Range(1, 4001),
      "s" -> Range(1, 4001)
    )

    countAcceptedWorkflow(workflows("in").conditions, workflows, parts)
  }

  println("Part 1: " + partOne(problemInput))
  println("Part 2: " + partTwo(problemInput))
}
