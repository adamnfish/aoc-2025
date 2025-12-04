package com.adamnfish.days

import cats.*
import cats.data.*
import cats.syntax.all.*
import cats.effect.IO
import com.adamnfish.Tools

object Day04 {
  def part1(inputFile: String) = {
    for {
      matrix <- Tools
        .inputLines("4", inputFile)
        .filter(_.nonEmpty)
        .map(_.toVector)
        .compile
        .toVector
        .map(padMatrix)
      reachableNumber = paddedNeighbourhoods(matrix)
        .count(isReachable)
    } yield reachableNumber.toString
  }

  def part2(inputFile: String) = {
    Tools.inputLines("4", inputFile).compile.lastOrError
  }

  def isReachable(neighbourhood: (Char, Coord, Vector[Char])): Boolean = {
    val (center, _, neighbours) = neighbourhood
    center == '@' && neighbours.count(_ == '@') < 4
  }

  def padMatrix(matrix: Vector[Vector[Char]]): Vector[Vector[Char]] = {
    val rows = matrix.length
    val cols = matrix.head.length
    Vector(Vector.fill(cols + 2)('.')) ++
      matrix.map('.' +: _ :+ '.') ++
      Vector(Vector.fill(cols + 2)('.'))
  }

  def paddedNeighbourhoods(
      matrix: Vector[Vector[Char]]
  ): Vector[(Char, Coord, Vector[Char])] = {
    for {
      r <- (1 until matrix.length - 1).toVector
      c <- (1 until matrix.head.length - 1).toVector
    } yield {
      val neighbours = Vector(
          // format: off
          matrix(r - 1)(c - 1), matrix(r - 1)(c), matrix(r - 1)(c + 1),
          matrix(r    )(c - 1), /* center */      matrix(r    )(c + 1),
          matrix(r + 1)(c - 1), matrix(r + 1)(c), matrix(r + 1)(c + 1)
          // format: on
        )
      (matrix(r)(c), Coord(r, c), neighbours)
    }
  }

  case class Coord(row: Int, col: Int)

  def debugPrintMatrix(matrix: Vector[Vector[Char]]): IO[Unit] = {
    matrix
      .map(row => row.mkString)
      .traverse_(line => IO.println(line))
  }
}
