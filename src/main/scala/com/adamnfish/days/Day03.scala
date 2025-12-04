package com.adamnfish.days

import cats.*
import cats.data.*
import cats.effect.IO
import cats.syntax.all.*
import com.adamnfish.Parsing.intoF
import com.adamnfish.{Parsing, Tools}

import scala.annotation.tailrec

object Day03 {
  def part1(input: String): IO[String] = {
    Tools
      .inputLines("3", input)
      .filter(_.nonEmpty)
      .evalMap(Parser.parseLine)
      .map(maxJoltage2)
      .scan(0)(_ + _)
      .compile
      .lastOrError
      .map(_.toString)
  }

  def part2(input: String): IO[String] = {
    Tools
      .inputLines("3", input)
      .filter(_.nonEmpty)
      .evalMap(Parser.parseLine)
      .map(maxJoltageN(12))
      .scan(BigInt(0))(_ + _)
      .compile
      .lastOrError
      .map(_.toString)
  }

  def maxJoltage2(bank: Bank): Int = {
    val firstMax = bank.batteries.init.max
    val lastMax =
      bank.batteries
        .dropWhile(_ != firstMax)
        .tail
        .max
    (firstMax * 10) + lastMax
  }

  def maxJoltageN(size: Int)(bank: Bank): BigInt = {
    @tailrec
    def loop(
        remainingSize: Int,
        batteries: Seq[Int],
        digits: List[Char]
    ): List[Char] = {
      if (remainingSize <= 0) {
        digits
      } else {
        val max = batteries.dropRight(remainingSize - 1).max
        val remainingBatteries = batteries
          .dropWhile(_ != max)
          .tail
        loop(remainingSize - 1, remainingBatteries, max.toString.head :: digits)
      }
    }
    val chars = loop(size, bank.batteries, Nil)
    BigInt(chars.reverse.mkString)
  }

  case class Bank(batteries: Seq[Int])

  object Parser {
    import fastparse.*
    import SingleLineWhitespace.*

    def parseLine(line: String): IO[Bank] =
      parse(line, lineParser(using _)).intoF

    def lineParser[$: P]: P[Bank] =
      P(
        Parsing.digit
          .rep()
          .map { case batteries => Bank(batteries) }
      )
  }
}
