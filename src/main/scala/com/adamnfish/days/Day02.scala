package com.adamnfish.days

import cats.*
import cats.data.*
import cats.syntax.all.*
import cats.effect.IO
import com.adamnfish.{Parsing, Tools}
import com.adamnfish.Parsing.intoF

object Day02 {
  def part1(input: String): IO[String] = {
    day2logic(input)(validate1)
  }

  def part2(input: String): IO[String] = {
    day2logic(input)(validate2)
  }

  def day2logic(input: String)(validator: BigInt => Id): IO[String] = {
    for {
      line <- Tools
        .inputLines("2", input)
        .filter(_.nonEmpty)
        .compile
        .lastOrError
      idRange <- Parser.parseLine(line)
      ids = idRange.flatMap(expandRange)
      validatedIds = ids.map(validator)
      invalidIds = validatedIds.collect { case Id.Invalid(id) =>
        id
      }
    } yield invalidIds.sum.toString
  }

  private val IsInvalidSillyPatterns =
    """^(\d+)\1$""".r

  private val IsInvalidOtherSillyPatterns =
    """^(\d+)\1+$""".r

  def validate1(id: BigInt): Id =
    id.toString match {
      case IsInvalidSillyPatterns(_) => Id.Invalid(id)
      case _                         => Id.Valid(id)
    }

  def validate2(id: BigInt): Id =
    id.toString match {
      case IsInvalidOtherSillyPatterns(_) => Id.Invalid(id)
      case _                              => Id.Valid(id)
    }

  case class IdRange(start: BigInt, end: BigInt)

  def expandRange(idRange: IdRange): Seq[BigInt] =
    idRange.start to idRange.end

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

    def invalidParser[$: P]: P[Seq[BigInt]] =
      P(
        Parsing.bigInt.rep(sep = ",")
      )
  }
}
