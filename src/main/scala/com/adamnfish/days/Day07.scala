package com.adamnfish.days

import cats.*
import cats.data.*
import cats.syntax.all.*
import cats.effect.IO
import com.adamnfish.Tools
import com.adamnfish.Parsing
import com.adamnfish.Parsing.*

object Day07 {
  def part1(inputFile: String) = {
    val inputStream = Tools.inputLines("7", inputFile)
    for {
      head <- inputStream.head.compile.lastOrError
      beamStarts = head.indexOf('S')
      (count, finalBeams) <- inputStream
        .map(_.toSeq)
        .map(indicesWhere[Char](_ == '^'))
        .map(_.toSet)
        .scan((0, Beams(Set(beamStarts)))) {
          case ((splitCount, activeBeams), row) =>
            val collisions = activeBeams.cols.intersect(row)
            val newBeams = collisions.foldRight(activeBeams) {
              (splitter, accBeams) =>
                accBeams.copy(
                  cols =
                    accBeams.cols - splitter + (splitter - 1) + (splitter + 1)
                )
            }
            (splitCount + collisions.size, newBeams)
        }
        .compile
        .lastOrError
    } yield count.toString
  }

  def part2(inputFile: String) = {
    val inputStream = Tools.inputLines("7", inputFile)
    for {
      head <- inputStream.head.compile.lastOrError
      beamStart = head.indexOf('S')
      finalBeamWeights <- inputStream
        .map(_.toSeq)
        .map(indicesWhere[Char](_ == '^'))
        .map(_.toSet)
        .scan(Map(beamStart -> 1L)) { case (activeBeams, row) =>
          val collisions = activeBeams.keySet.intersect(row)
          val newBeams = collisions
            .foldRight(activeBeams) { (splitter, accBeams) =>
              val countAtSplitter = accBeams(splitter)
              accBeams
                .removed(splitter)
                .updatedWith(splitter - 1) { existingOpt =>
                  Some(existingOpt.getOrElse(0L) + countAtSplitter)
                }
                .updatedWith(splitter + 1) { existingOpt =>
                  Some(existingOpt.getOrElse(0L) + countAtSplitter)
                }
            }
          newBeams
        }
        .compile
        .lastOrError
      count = finalBeamWeights.values.sum
    } yield count.toString
  }

  case class Row(splitters: Seq[Int])
  case class Beams(cols: Set[Int])

  def indicesWhere[T](pred: T => Boolean)(seq: Seq[T]): Seq[Int] =
    seq.zipWithIndex.collect { case (elem, idx) if pred(elem) => idx }
}
