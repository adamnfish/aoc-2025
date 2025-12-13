package com.adamnfish.days

import cats.*
import cats.data.*
import cats.syntax.all.*
import cats.effect.IO
import com.adamnfish.Tools
import fs2.Pipe

import scala.annotation.tailrec

object Day04 {
  def part1(inputFile: String) = {
    for {
      grid <- Tools
        .inputLines("4", inputFile)
        .through(generateGrid)
        .compile
        .lastOrError
      neighbourhoods = paperNeighbourhoods(grid)
      reachableNumber = neighbourhoods
        .count(isReachable)
    } yield reachableNumber.toString
  }

  def part2(inputFile: String) = {
    for {
      grid <- Tools
        .inputLines("4", inputFile)
        .through(generateGrid)
        .compile
        .lastOrError
      (finalGrid, eliminatedCount) = repeatedlyEliminateReachable(grid, 0)
    } yield eliminatedCount
  }

  /** Turns a stream of lines into our Grid data structure.
    *
    * Counts the rows as we go, and keeps track of the maximum column width.
    */
  private val generateGrid: Pipe[IO, String, Grid] = { stream =>
    stream
      .scan((0, -1, Map.empty[Coord, Char])) {
        case ((rowIndex, maxCol, acc), line) =>
          (
            rowIndex + 1,
            math.max(maxCol, line.length),
            acc ++ parseLine(line, rowIndex)
          )
      }
      .map { case (maxRowIndex, maxWidth, cells) =>
        Grid(maxWidth, maxRowIndex, cells)
      }
  }

  private def parseLine(line: String, rowIndex: Int): Map[Coord, Char] = {
    line.zipWithIndex.map { case (char, colIndex) =>
      Coord(rowIndex, colIndex) -> char
    }.toMap
  }

  @tailrec
  private def repeatedlyEliminateReachable(
      grid: Grid,
      eliminatedCount: Int
  ): (Grid, Int) = {
    val neighbourhoods = paperNeighbourhoods(grid)
    val toEliminate = neighbourhoods
      .filter(isReachable)
      .map(_._1)
      .toSet
    if (toEliminate.isEmpty) {
      // we're finished removing reachable paper rolls
      (grid, eliminatedCount)
    } else {
      val newCells = grid.cells.map { case (coord, char) =>
        if (toEliminate.contains(coord)) {
          coord -> '.'
        } else {
          coord -> char
        }
      }
      val newGrid = grid.copy(cells = newCells)
      repeatedlyEliminateReachable(newGrid, eliminatedCount + toEliminate.size)
    }
  }

  private def paperNeighbourhoods(
      grid: Grid
  ): Iterable[(Coord, Vector[Char])] = {
    grid.cells
      .filter { case (_, char) => char == '@' }
      .map { (coord, char) =>
        (coord, neighbourhood(coord, grid))
      }
  }

  private def neighbourhood(
      coord: Coord,
      grid: Grid
  ): Vector[Char] = {
    def getCell(rOffset: Int, cOffset: Int): Char = {
      grid.cells.getOrElse(
        Coord(coord.row + rOffset, coord.col + cOffset),
        default = '.'
      )
    }
    Vector(
      // format: off
      getCell(-1, -1), getCell(-1, 0), getCell(-1, 1),
      getCell( 0, -1),  /* center */   getCell( 0, 1),
      getCell( 1, -1), getCell( 1, 0), getCell( 1, 1)
      // format: on
    )
  }

  private def isReachable(neighbourhood: (Coord, Vector[Char])): Boolean = {
    neighbourhood._2.count(_ == '@') < 4
  }

  case class Coord(row: Int, col: Int)
  case class Grid(width: Int, height: Int, cells: Map[Coord, Char])

  def debugPrintMatrix(matrix: Vector[Vector[Char]]): IO[Unit] = {
    matrix
      .map(row => row.mkString)
      .traverse_(line => IO.println(line))
  }
}
