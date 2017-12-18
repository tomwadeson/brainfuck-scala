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

  val incrementPointer: Parser[Instruction]             = char('>') >| IncrementPointer
  val decrementPointer: Parser[Instruction]             = char('<') >| DecrementPointer
  val incrementValueAtPointer: Parser[Instruction]      = char('+') >| IncrementValueAtPointer
  val decrementValueAtPointer: Parser[Instruction]      = char('-') >| DecrementValueAtPointer
  val outputValueAtPointer: Parser[Instruction]         = char('.') >| OutputValueAtPointer
  val readAndSetValueAtPointer: Parser[Instruction]     = char(',') >| ReadAndSetValueAtPointer
  val doWhileValueAtPointerNonZero: Parser[Instruction] = squareBrackets(program) -| DoWhileValueAtPointerNonZero

  val instruction: Parser[Instruction] = choice(
    incrementPointer,
    decrementPointer,
    incrementValueAtPointer,
    decrementValueAtPointer,
    outputValueAtPointer,
    readAndSetValueAtPointer,
    doWhileValueAtPointerNonZero
  )

  private val skipOthers            = skipMany(noneOf("><+-.,[]"))
  lazy val program: Parser[Program] = ((many(skipOthers ~> instruction) <~ skipOthers) -| Program).named("Program")
}
