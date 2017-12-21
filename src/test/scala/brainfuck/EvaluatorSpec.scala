package brainfuck

import cats.data.{NonEmptyList, State, StateT}
import cats.mtl.implicits._
import org.scalatest.{FunSuite, Matchers}

class EvaluatorSpec extends FunSuite with Matchers {

  test("Evaluate a 'Hello World!' Brainfuck program") {
    val program = Parser.parse(helloWorldSource).right.get

    val (consoleState, _) =
      Evaluator.evaluate[BrainfuckTest](program).run(Machine.sized(10)).run(ConsoleState.initial).value

    consoleState.stdOut.map(_.toChar).mkString shouldBe "Hello World!\n"
  }

  private val helloWorldSource =
    """
      |[ This program prints "Hello World!" and a newline to the screen, its
      |  length is 106 active command characters. [It is not the shortest.]
      |
      |  This loop is an "initial comment loop", a simple way of adding a comment
      |  to a BF program such that you don't have to worry about any command
      |  characters. Any ".", ",", "+", "-", "<" and ">" characters are simply
      |  ignored, the "[" and "]" characters just have to be balanced. This
      |  loop and the commands it contains are ignored because the current cell
      |  defaults to a value of 0; the 0 value causes this loop to be skipped.
      |]
      |++++++++               Set Cell #0 to 8
      |[
      |    >++++               Add 4 to Cell #1; this will always set Cell #1 to 4
      |    [                   as the cell will be cleared by the loop
      |        >++             Add 2 to Cell #2
      |        >+++            Add 3 to Cell #3
      |        >+++            Add 3 to Cell #4
      |        >+              Add 1 to Cell #5
      |        <<<<-           Decrement the loop counter in Cell #1
      |    ]                   Loop till Cell #1 is zero; number of iterations is 4
      |    >+                  Add 1 to Cell #2
      |    >+                  Add 1 to Cell #3
      |    >-                  Subtract 1 from Cell #4
      |    >>+                 Add 1 to Cell #6
      |    [<]                 Move back to the first zero cell you find; this will
      |                        be Cell #1 which was cleared by the previous loop
      |    <-                  Decrement the loop Counter in Cell #0
      |]                       Loop till Cell #0 is zero; number of iterations is 8
      |
      |The result of this is:
      |Cell No :   0   1   2   3   4   5   6
      |Contents:   0   0  72 104  88  32   8
      |Pointer :   ^
      |
      |>>.                     Cell #2 has value 72 which is 'H'
      |>---.                   Subtract 3 from Cell #3 to get 101 which is 'e'
      |+++++++..+++.           Likewise for 'llo' from Cell #3
      |>>.                     Cell #5 is 32 for the space
      |<-.                     Subtract 1 from Cell #4 for 87 to give a 'W'
      |<.                      Cell #3 was set to 'o' from the end of 'Hello'
      |+++.------.--------.    Cell #3 for 'rl' and 'd'
      |>>+.                    Add 1 to Cell #5 gives us an exclamation point
      |>++.                    And finally a newline from Cell #6
    """.stripMargin

  type MockConsoleIO[A] = State[ConsoleState, A]
  type BrainfuckTest[A] = StateT[MockConsoleIO, Machine, A]

  implicit val mockConsoleIO: Console[BrainfuckTest] = new Console[BrainfuckTest] {
    override def readByte(): BrainfuckTest[Byte] =
      for {
        s <- StateT.lift(State.get[ConsoleState])
        bs = s.stdIn
        _ <- StateT.lift[MockConsoleIO, Machine, Unit](
          State.set[ConsoleState](s.copy(stdIn = NonEmptyList.fromList(bs.tail).getOrElse(bs))))
      } yield bs.head

    override def writeByte(byte: Byte): BrainfuckTest[Unit] =
      StateT.lift(State.modify[ConsoleState](s => s.copy(stdOut = s.stdOut :+ byte)))
  }
}

final case class ConsoleState(stdIn: NonEmptyList[Byte], stdOut: List[Byte])

object ConsoleState {
  val initial: ConsoleState = ConsoleState(NonEmptyList(0, Nil), Nil)
}
