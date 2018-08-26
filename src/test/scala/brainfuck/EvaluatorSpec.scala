package brainfuck

import brainfuck.EvaluatorSpec.BrainfuckTest
import cats.Monad
import cats.data.State
import cats.implicits._
import cats.mtl.implicits._
import cats.mtl.{DefaultMonadState, MonadState}
import io.estatico.newtype.macros.newtype
import org.scalatest.{FreeSpec, Matchers}

class EvaluatorSpec extends FreeSpec with Matchers {

  "Evaluate a 'Hello World!' Brainfuck program" in {
    val program = Parser.parse(helloWorldSource).right.get

    val ((_, consoleState), _) =
      Evaluator.evaluate[BrainfuckTest](program).runBrainfuckTest((Machine.sized(10), ConsoleState.initial))

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
}

object EvaluatorSpec {

  @newtype
  final case class BrainfuckTest[A](run: State[(Machine, ConsoleState), A]) {
    def runBrainfuckTest(withInitialState: (Machine, ConsoleState)): ((Machine, ConsoleState), A) =
      run.run(withInitialState).value
  }

  object BrainfuckTest {

    implicit val monad: Monad[BrainfuckTest] = derivingK

    private val monadStateMachineConsole: MonadState[BrainfuckTest, (Machine, ConsoleState)] =
      derivingK[MonadState[?[_], (Machine, ConsoleState)]]

    implicit val monadStateMachine: MonadState[BrainfuckTest, Machine] =
      new DefaultMonadState[BrainfuckTest, Machine] {
        override val monad: Monad[BrainfuckTest] = implicitly
        override def get: BrainfuckTest[Machine] = monadStateMachineConsole.get.map(_._1)
        override def set(s: Machine): BrainfuckTest[Unit] =
          for {
            (_, consoleState) <- monadStateMachineConsole.get
            _                 <- monadStateMachineConsole.set((s, consoleState))
          } yield ()
      }

    implicit val console: Console[BrainfuckTest] = new Console[BrainfuckTest] {

      override def readByte(): BrainfuckTest[Byte] =
        for {
          (machine, consoleState) <- monadStateMachineConsole.get
          byte = consoleState.stdIn.head
          _ <- monadStateMachineConsole.set((machine, consoleState.copy(stdIn = consoleState.stdIn.tail)))
        } yield byte

      override def writeByte(byte: Byte): BrainfuckTest[Unit] =
        monadStateMachineConsole.modify { case (machine, consoleState) =>
            (machine, consoleState.copy(stdOut = consoleState.stdOut :+ byte))
        }
    }
  }
}

final case class ConsoleState(stdIn: List[Byte], stdOut: List[Byte])

object ConsoleState {
  val initial: ConsoleState = ConsoleState(Nil, Nil)
}
