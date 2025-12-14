package com.adamnfish.days

import cats.*
import cats.data.*
import cats.syntax.all.*
import cats.effect.IO
import com.adamnfish.Tools
import com.adamnfish.Parsing
import com.adamnfish.Parsing.*

object Day08 {
  def part1(inputFile: String) = {
    val pairsCount = inputFile match {
      case "example" => 10
      case _         => 1000
    }
    for {
      boxes <- Tools
        .inputLines("8", inputFile)
        .filterNot(_.isEmpty)
        .evalMap(Parser.parseLine)
        .compile
        .toVector
      boxPairs <- boxes.combinations(2).toSeq.traverse {
        case Seq(a, b) =>
          IO.pure((a, b))
        case _ =>
          IO.raiseError(new Exception("Impossible"))
      }
      initialCircuits = boxes.toSet.map(box => Set(box))
      sortedByDistance = boxPairs
        .sortBy(pairwiseMagnitude.tupled)
        .take(pairsCount)
      finalCircuits = doConnections(sortedByDistance, initialCircuits)
    } yield threeLargestProduct(finalCircuits).toString
  }

  def part2(inputFile: String) = {
    val pairsCount = inputFile match {
      case "example" => 10
      case _         => 1000
    }
    for {
      boxes <- Tools
        .inputLines("8", inputFile)
        .filterNot(_.isEmpty)
        .evalMap(Parser.parseLine)
        .compile
        .toVector
      boxPairs <- boxes.combinations(2).toSeq.traverse {
        case Seq(a, b) =>
          IO.pure((a, b))
        case _ =>
          IO.raiseError(new Exception("Impossible"))
      }
      initialCircuits = boxes.toSet.map(box => Set(box))
      sortedByDistance = boxPairs
        .sortBy(pairwiseMagnitude.tupled)
      last = finalConnection(sortedByDistance, initialCircuits)
    } yield (last._1.x * last._2.x).toString
  }

  case class Coord3(x: Int, y: Int, z: Int)

  def doConnections(
      connections: Seq[(Coord3, Coord3)],
      initialCircuits: Set[Set[Coord3]]
  ): Set[Set[Coord3]] = {
    connections.foldLeft(initialCircuits)(applyConnection)
  }

  def finalConnection(
      allOrderedConnections: Seq[(Coord3, Coord3)],
      initialCircuits: Set[Set[Coord3]]
  ): (Coord3, Coord3) = {
    def loop(
        remainingConnections: Seq[(Coord3, Coord3)],
        circuits: Set[Set[Coord3]]
    ): (Coord3, Coord3) = {
      remainingConnections match {
        case Nil =>
          throw new Exception("No final connection found")
        case (c1, c2) +: tail =>
          val newCircuits = applyConnection(circuits, (c1, c2))
          if (newCircuits.size == 1) {
            (c1, c2)
          } else {
            loop(tail, newCircuits)
          }
      }
    }
    loop(allOrderedConnections, initialCircuits)
  }

  def applyConnection(
    circuits: Set[Set[Coord3]],
    connection: (Coord3, Coord3)
  ): Set[Set[Coord3]] = {
    val (c1, c2) = connection
    val connect1Circuits = circuits.filter(_.contains(c1)).flatten
    val connect2Circuits = circuits.filter(_.contains(c2)).flatten
    val newCircuits = connect1Circuits.union(connect2Circuits)
    val remainingCircuits =
      circuits.filterNot(c => c.contains(c1) || c.contains(c2))
    remainingCircuits + newCircuits
  }

  def threeLargestProduct(
      circuits: Set[Set[Coord3]]
  ): BigInt = {
    val sortedSizes =
      circuits.toSeq.map(_.size).sorted(using Ordering.Int.reverse)
    sortedSizes.take(3).map(BigInt(_)).product
  }

  // no need to sqrt it, just need relative distances
  def pairwiseMagnitude(a: Coord3, b: Coord3): Double = {
    math.pow(b.x - a.x, 2) +
      math.pow(b.y - a.y, 2) +
      math.pow(b.z - a.z, 2)
  }

  object Parser {
    import fastparse.*, NoWhitespace.*

    def parseLine(line: String): IO[Coord3] =
      parse(line, lineParser(using _)).intoF

    def lineParser[$: P]: P[Coord3] =
      P(
        (Parsing.integer ~ "," ~ Parsing.integer ~ "," ~ Parsing.integer)
          .map { case (x, y, z) => Coord3(x, y, z) }
      )
  }
}
