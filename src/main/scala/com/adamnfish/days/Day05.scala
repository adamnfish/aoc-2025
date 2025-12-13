package com.adamnfish.days

import cats.*
import cats.data.*
import cats.syntax.all.*
import cats.effect.IO
import com.adamnfish.Tools
import com.adamnfish.Parsing
import com.adamnfish.Parsing.*

object Day05 {
  def part1(inputFile: String) = {
    for {
      inputLines <- Tools
        .inputLines("5", inputFile)
        .compile
        .toVector
      ranges <- inputLines
        .takeWhile(_.nonEmpty)
        .traverse(Parser.parseRange)
      ids <- inputLines
        .dropWhile(_.nonEmpty)
        .drop(1)
        .filterNot(_.isEmpty)
        .traverse(Parser.parseId)
      freshIdsCount = ids.count(id => idIsFresh(id, ranges))
    } yield freshIdsCount.toString
  }

  def part2(inputFile: String) = {
    for {
      inputLines <- Tools
        .inputLines("5", inputFile)
        .compile
        .toVector
      ranges <- inputLines
        .takeWhile(_.nonEmpty)
        .traverse(Parser.parseRange)
      allFreshIds = ranges
        .flatMap(expandRange)
        .toSet
    } yield allFreshIds.size.toString
  }

  def idIsFresh(id: BigInt, ranges: Seq[Range]): Boolean = {
    ranges.exists { range =>
      id >= range.start && id <= range.end
    }
  }

  def expandRange(range: Range): Set[BigInt] = {
    (range.start to range.end).toSet
  }

  case class Range(start: BigInt, end: BigInt)

  object Parser {
    import fastparse.*, NoWhitespace.*

    def parseId(line: String): IO[BigInt] =
      parse(line, idParser(using _)).intoF

    def idParser[$: P]: P[BigInt] =
      P(Parsing.bigInt)

    def parseRange(line: String): IO[Range] =
      parse(line, rangeParser(using _)).intoF

    def rangeParser[$: P]: P[Range] =
      P(
        Parsing.bigInt ~ "-" ~ Parsing.bigInt
      ).map { case (start, end) =>
        Range(start, end)
      }
  }
}
