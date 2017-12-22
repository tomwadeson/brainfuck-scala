package brainfuck

import atto.Atto._
import atto._
import brainfuck.Instruction._

object Parser {

  /**
    * Parse Brainfuck sourcecode into an AST
    *
    * >>> Parser.parse("><+-.,").right.get
    * Program(List(IncrementPointer, DecrementPointer, IncrementCurrentRegister, DecrementCurrentRegister, OutputCurrentRegister, ReadAndSetCurrentRegister))
    *
    * >>> Parser.parse("[-]").right.get
    * Program(List(DoWhileCurrentRegisterNonZero(Program(List(DecrementCurrentRegister)))))
    *
    * >>> Parser.parse("[-[+]]").right.get
    * Program(List(DoWhileCurrentRegisterNonZero(Program(List(DecrementCurrentRegister, DoWhileCurrentRegisterNonZero(Program(List(IncrementCurrentRegister))))))))
    */
  def parse(str: String): Either[String, Program] = program.parseOnly(str).either

  val incrementPointer: Parser[Instruction]              = char('>') >| IncrementPointer
  val decrementPointer: Parser[Instruction]              = char('<') >| DecrementPointer
  val incrementCurrentRegister: Parser[Instruction]      = char('+') >| IncrementCurrentRegister
  val decrementCurrentRegister: Parser[Instruction]      = char('-') >| DecrementCurrentRegister
  val outputCurrentRegister: Parser[Instruction]         = char('.') >| OutputCurrentRegister
  val readAndSetCurrentRegister: Parser[Instruction]     = char(',') >| ReadAndSetCurrentRegister
  val doWhileCurrentRegisterNonZero: Parser[Instruction] = squareBrackets(program) -| DoWhileCurrentRegisterNonZero

  val instruction: Parser[Instruction] = choice(
    incrementPointer,
    decrementPointer,
    incrementCurrentRegister,
    decrementCurrentRegister,
    outputCurrentRegister,
    readAndSetCurrentRegister,
    doWhileCurrentRegisterNonZero
  )

  private val skipOthers            = skipMany(noneOf("><+-.,[]"))
  lazy val program: Parser[Program] = ((many(skipOthers ~> instruction) <~ skipOthers) -| Program).named("Program")
}
