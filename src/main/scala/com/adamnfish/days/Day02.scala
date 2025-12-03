package com.adamnfish.days

import cats.*
import cats.data.*
import cats.syntax.all.*
import cats.effect.IO
import com.adamnfish.{Parsing, Tools}
import com.adamnfish.Parsing.intoF
import scala.util.matching.Regex

object Day02 {
  def part1(input: String): IO[String] = {
    day2logic(input, IsInvalidSillyPatterns)
  }

  def part2(input: String): IO[String] = {
    day2logic(input, IsInvalidOtherSillyPatterns)
  }

  val IsInvalidSillyPatterns =
    """^(\d+)\1$""".r

  val IsInvalidOtherSillyPatterns =
    """^(\d+)\1+$""".r

  def validate(id: BigInt, badIdCheck: Regex): Id =
    id.toString match {
      case badIdCheck(_) => Id.Invalid(id)
      case _             => Id.Valid(id)
    }

  def day2logic(input: String, badIdCheck: Regex): IO[String] = {
    for {
      line <- Tools
        .inputLines("2", input)
        .filter(_.nonEmpty)
        .compile
        .lastOrError
      idRange <- Parser.parseLine(line)
      ids = idRange.flatMap(expandRange)
      validatedIds = ids.map(validate(_, badIdCheck))
      invalidIds = validatedIds.collect { case Id.Invalid(id) =>
        id
      }
    } yield invalidIds.sum.toString
  }

  private def expandRange(idRange: IdRange): Seq[BigInt] =
    idRange.start to idRange.end

  case class IdRange(start: BigInt, end: BigInt)

  enum Id {
    case Valid(id: BigInt)
    case Invalid(id: BigInt)
  }

  object Parser {
    import fastparse.*, SingleLineWhitespace.*

    def parseLine(line: String): IO[Seq[IdRange]] =
      parse(line, lineParser(using _)).intoF

    def lineParser[$: P]: P[Seq[IdRange]] =
      P(
        (Parsing.bigInt ~ "-" ~ Parsing.bigInt)
          .map { case (start, end) => IdRange(start, end) }
          .rep(sep = ",")
      )
  }
}
