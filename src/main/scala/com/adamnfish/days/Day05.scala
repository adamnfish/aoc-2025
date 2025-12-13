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
    } yield resolveRanges(ranges).toString
  }

  def idIsFresh(id: BigInt, ranges: Seq[Range]): Boolean = {
    ranges.exists { range =>
      id >= range.start && id <= range.end
    }
  }

  def resolveRanges(ranges: Vector[Range]): BigInt = {
    val (count, _) = ranges
      .sortBy(_.start)
      .foldLeft(BigInt(0), BigInt(0)) { case ((count, pointer), range) =>
        if (range.end < pointer) { // If the range is entirely before our pointer, ignore it
          (count, pointer)
        } else if (range.start <= pointer) { // Overlaps with our pointer, extend to the end
          val newCount = count + (range.end - pointer + 1)
          (newCount, range.end + 1)
        } else { // Starts after our pointer, add the whole range
          val newCount = count + range.size
          (newCount, range.end + 1)
        }
      }
    count
  }

  case class Range(start: BigInt, end: BigInt) {
    def size: BigInt = end - start + 1
  }

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
