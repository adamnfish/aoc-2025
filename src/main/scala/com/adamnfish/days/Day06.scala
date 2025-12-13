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
        .map { case (op, ops) => Operation(op, ops) }
        .map(calculate)
    } yield answers.sum.toString
  }

  def part2(inputFile: String) = {
    for {
      inputLines <- Tools
        .inputLines("6", inputFile)
        .filter(_.nonEmpty)
        .compile
        .toVector
      operations = parseInputLines(
        inputLines(0).toList,
        inputLines(1).toList,
        inputLines(2).toList,
        inputLines(3).toList,
        inputLines(4).toList
      )
    } yield operations.map(calculate).sum.toString
  }

  // PART 1

  object Parser {
    import fastparse.*, NoWhitespace.*

    def parseOperands(line: String): IO[Seq[BigInt]] =
      parse(line, operandsParser(using _)).intoF

    def operandsParser[$: P]: P[Seq[BigInt]] =
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

  // PART 2

  case class Operation(
      operator: Operator,
      operands: Seq[BigInt]
  )

  def calculate(operation: Operation): BigInt = {
    operation.operator match {
      case Operator.Add      => operation.operands.sum
      case Operator.Multiply => operation.operands.product
    }
  }

  def parseInputLines(
      n1s: List[Char],
      n2s: List[Char],
      n3s: List[Char],
      n4s: List[Char],
      ops: List[Char]
  ): Seq[Operation] = {
    def loop(
        n1s: List[Char],
        n2s: List[Char],
        n3s: List[Char],
        n4s: List[Char],
        ops: List[Char],
        acc: Seq[Operation],
        currentOperation: Option[Operation]
    ): Seq[Operation] = {
      (n1s, n2s, n3s, n4s, ops) match {
        case (
              n1c :: n1Rest,
              n2c :: n2Rest,
              n3c :: n3Rest,
              n4c :: n4Rest,
              opc :: opRest
            ) =>
          val digits = Seq(n1c, n2c, n3c, n4c)
            .filter(_.isDigit)
            .mkString("")

          (opc, currentOperation) match {
            case ('+', None) => // starting a new sum operation
              val operand = digits.toInt
              loop(
                n1Rest,
                n2Rest,
                n3Rest,
                n4Rest,
                opRest,
                acc,
                Some(Operation(Operator.Add, Seq(operand)))
              )
            case ('*', None) => // starting a new product operation
              val operand = digits.toInt
              loop(
                n1Rest,
                n2Rest,
                n3Rest,
                n4Rest,
                opRest,
                acc,
                Some(Operation(Operator.Multiply, Seq(operand)))
              )
            case (' ', Some(current)) if digits.nonEmpty => // adding an operand to the current operation
              val operand = digits.toInt
              loop(
                n1Rest,
                n2Rest,
                n3Rest,
                n4Rest,
                opRest,
                acc,
                Some(current.copy(operands = current.operands :+ operand))
              )
            case (' ', Some(current)) => // end of an operation
              loop(n1Rest, n2Rest, n3Rest, n4Rest, opRest, acc :+ current, None)
            case unexpected =>
              throw new RuntimeException(s"Unexpected case: $unexpected")
          }
        case _ =>
          // exhausted input, so let's close the current operation off and exit
          currentOperation match {
            case Some(current) => acc :+ current
            case None          => acc
          }
      }
    }
    loop(n1s, n2s, n3s, n4s, ops, Seq.empty, None)
  }

  // Shared

  enum Operator {
    case Add
    case Multiply
  }
}
