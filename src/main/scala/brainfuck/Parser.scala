package brainfuck

import atto.Atto._
import atto._
import brainfuck.Instruction._

object Parser {

  /**
    * Parse Brainfuck sourcecode into an AST
    *
    * >>> Parser.parse("><+-.,").right.get
    * Program(List(IncrementPointer, DecrementPointer, IncrementValueAtPointer, DecrementValueAtPointer, OutputValueAtPointer, ReadAndSetValueAtPointer))
    *
    * >>> Parser.parse("[-]").right.get
    * Program(List(DoWhileValueAtPointerNonZero(Program(List(DecrementValueAtPointer)))))
    *
    * >>> Parser.parse("[-[+]]").right.get
    * Program(List(DoWhileValueAtPointerNonZero(Program(List(DecrementValueAtPointer, DoWhileValueAtPointerNonZero(Program(List(IncrementValueAtPointer))))))))
    */
  def parse(str: String): Either[String, Program] = program.parseOnly(str).either

  private val incrementPointer: Parser[Instruction]         = char('>') >| IncrementPointer
  private val decrementPointer: Parser[Instruction]         = char('<') >| DecrementPointer
  private val incrementValueAtPointer: Parser[Instruction]  = char('+') >| IncrementValueAtPointer
  private val decrementValueAtPointer: Parser[Instruction]  = char('-') >| DecrementValueAtPointer
  private val outputValueAtPointer: Parser[Instruction]     = char('.') >| OutputValueAtPointer
  private val readAndSetValueAtPointer: Parser[Instruction] = char(',') >| ReadAndSetValueAtPointer
  private val doWhileValueAtPointerNonZero
    : Parser[Instruction] = squareBrackets(program) -| DoWhileValueAtPointerNonZero

  private val skipOthers = skipMany(noneOf("><+-.,[]"))

  private val instruction = List(
    incrementPointer,
    decrementPointer,
    incrementValueAtPointer,
    decrementValueAtPointer,
    outputValueAtPointer,
    readAndSetValueAtPointer,
    doWhileValueAtPointerNonZero
  ).map(skipOthers ~> _).reduce(_ | _)

  private lazy val program = skipOthers ~> many(instruction) -| Program
}
