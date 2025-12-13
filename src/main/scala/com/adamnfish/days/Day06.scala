package com.adamnfish.days

import cats.*
import cats.data.*
import cats.syntax.all.*
import cats.effect.IO
import com.adamnfish.Tools
import com.adamnfish.Parsing
import com.adamnfish.Parsing.*

object Day06 {
  def part1(inputFile: String) = {
    for {
      inputLines <- Tools
        .inputLines("6", inputFile)
        .filter(_.nonEmpty)
        .compile
        .toVector
      operands <- inputLines.init
        .traverse(Parser.parseOperands)
        .map(_.transpose)
      operators <- Parser.parseOperators(inputLines.last)
      answers = operators
        .zip(operands)
        .map(calculate)
    } yield answers.sum.toString
  }

  def part2(inputFile: String) = {
    Tools.inputLines("6", inputFile).compile.lastOrError
  }

  def calculate(operator: Operator, operands: Seq[BigInt]): BigInt = {
    operator match {
      case Operator.Add      => operands.sum
      case Operator.Multiply => operands.product
    }
  }

  enum Operator {
    case Add
    case Multiply
  }

  object Parser {
    import fastparse.*, NoWhitespace.*

    def parseOperands(line: String): IO[Seq[BigInt]] =
      parse(line, OperandsParser(using _)).intoF

    def OperandsParser[$: P]: P[Seq[BigInt]] =
      P(" ".rep(min = 0) ~ Parsing.bigInt.rep(sep = " ".rep) ~ " ".rep(min = 0))

    def parseOperators(line: String): IO[Seq[Operator]] =
      parse(line, operatorsParser(using _)).intoF

    def operatorsParser[$: P]: P[Seq[Operator]] =
      P(
        " ".rep(min = 0) ~ CharIn("+*").!.map {
          case "+" => Operator.Add
          case "*" => Operator.Multiply
        }.rep(sep = " ".rep) ~ " ".rep(min = 0)
      )
  }
}
