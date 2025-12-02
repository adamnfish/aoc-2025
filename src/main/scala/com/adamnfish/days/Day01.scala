package com.adamnfish.days

import cats.*
import cats.data.*
import cats.syntax.all.*
import cats.effect.IO
import com.adamnfish.{Parsing, Tools}
import com.adamnfish.Parsing.intoF

object Day01 {
  val size = 99 + 1

  def part1(inputFile: String): IO[String] = {
    for {
      count <- Tools
        .inputLines("1", inputFile)
        .filter(_.nonEmpty)
        .evalMap(Parser.parseLine)
        .scan(50)((position, rotation) => applyRotation(position, rotation))
        .scan(0)((count, position) => if (position == 0) count + 1 else count)
        .compile
        .lastOrError
    } yield count.toString
  }

  def part2(inputFile: String) = {
    for {
      count <- Tools
        .inputLines("1", inputFile)
        .filter(_.nonEmpty)
        .evalMap(Parser.parseLine)
        // start at position 50, with no zeros passed
        .scan((50, 0)) { case ((position, count), rotation) =>
          val newPosition = applyRotation(position, rotation)
          val additionalZeroes = countZeroesPassed(position, rotation)
          (newPosition, count + additionalZeroes)
        }
        .map(_._2)
        .compile
        .lastOrError
    } yield count.toString
  }

  def applyRotation(
      position: Int,
      rotation: Rotation
  ): Int = {
    val newPosition = rotation.direction match {
      case Direction.Left  => position - rotation.distance
      case Direction.Right => position + rotation.distance
    }
    // Constrain to the lock size (wraps around at 0 and size)
    ((newPosition % size) + size) % size
  }

  def countZeroesPassed(
      position: Int,
      rotation: Rotation
  ): Int = {
    // how many full rotations are in the distance
    val fullRotations = rotation.distance / size
    // maybe we pass an extra zero in the partial rotation
    // (only if there is a partial rotation - i.e. remainder > 0)
    val remainder = rotation.distance % size
    val partialRotationExtra = rotation.direction match {
      case Direction.Left =>
        if (remainder > 0 && position > 0 && position - remainder <= 0)
          1
        else 0
      case Direction.Right =>
        if (remainder > 0 && position + remainder >= size)
          1
        else 0
    }
    fullRotations + partialRotationExtra
  }
  
  case class Rotation(
    direction: Direction,
    distance: Int
  )

  enum Direction {
    case Left
    case Right
  }

  object Parser {
    import fastparse.*, SingleLineWhitespace.*

    def parseLine(line: String): IO[Rotation] =
      parse(line, lineParser(using _)).intoF

    def lineParser[$: P]: P[Rotation] =
      P(
        (CharIn("LR").! ~ Parsing.integer).map {
          case ("L", dist) => Rotation(Direction.Left, dist)
          case ("R", dist) => Rotation(Direction.Right, dist)
          case (c, _)      =>
            throw RuntimeException(s"Unexpected direction char in input: $c")
        }
      )
  }
}
